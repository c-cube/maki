
(* This file is free software. See file "license" for more details. *)

(** {1 Maki-Build}

    This tool is a {b TOY} build system for OCaml, intended only for
    demonstrating features of Maki. *)

open Lwt.Infix

module A = Oasis_ast
module P = Oasis_parser
module V = Maki.Value

(** {2 Helpers} *)

let fail = failwith
let failf msg = CCFormat.ksprintf msg ~f:fail
let failf_lwt msg = CCFormat.ksprintf msg ~f:(fun msg -> Lwt.fail (Failure msg))

let pp_string = CCFormat.string
let pp_list ~sep pp = CCFormat.(hbox (list ~start:"" ~stop:"" ~sep pp))
let pp_strings = pp_list ~sep:"," pp_string
let pp_strings_space = pp_list ~sep:" " pp_string

(* replace file extension of [a] *)
let set_ext a ~ext =
  try (Filename.chop_extension a) ^ ext
  with _ -> failf "could not chop extension of %s" a

let shellf msg =
  CCFormat.ksprintf msg
    ~f:(fun cmd ->
      Maki_log.logf 5 (fun k->k "run command `%s`" cmd);
      Maki.shell cmd)

(** {2 Basic building blocks} *)

(* path+module name --> filename *)
let module_to_ml ~path m =
  let n1 = Printf.sprintf "%s/%s.ml" path (String.uncapitalize m) in
  let n2 = Printf.sprintf "%s/%s.ml" path (String.capitalize m) in
  if CCIO.File.exists n1 then n1
  else if CCIO.File.exists n2 then n2
  else failf "could not find .ml file for module %s/%s" path m

let module_to_mli ~path m =
  let n1 = Printf.sprintf "%s/%s.mli" path (String.uncapitalize m) in
  let n2 = Printf.sprintf "%s/%s.mli" path (String.capitalize m) in
  if CCIO.File.exists n1 then n1
  else if CCIO.File.exists n2 then n2
  else module_to_ml ~path m (* fall back to .ml *)

let module_to_cmi ~path m =
  module_to_mli ~path m |> set_ext ~ext:".cmi"

let module_to_cmo ~path m =
  module_to_ml ~path m |> set_ext ~ext:".cmo"

let deps_to_args deps =
  let l = CCList.flat_map (fun d -> ["-package"; d]) deps in
  if List.mem "threads" deps then "-thread" :: l else l

(* other modules [m] depends on *)
let find_deps ~deps ~path m : string list Lwt.t =
  let file = module_to_ml ~path m in
  let pdeps = CCList.flat_map (fun d -> ["-package"; d]) deps in
  (* call "ocamldep" *)
  Maki.call_exn ~name:"find_deps"
    ~deps:[V.argv0; V.pack_file file; V.pack_set V.string pdeps]
    ~op:V.(set string)
    (fun () ->
       shellf "@[<h>ocamlfind ocamldep -modules %a %s@]"
         pp_strings_space pdeps file
       >|= fun (out,_,_) ->
       out
       |> CCString.Split.left_exn ~by:":"
       |> snd
       |> CCString.Split.list_cpy ~by:" "
       |> List.map String.trim
       |> List.filter (fun s -> s<>"")
    )
  >|= fun l ->
  Maki_log.logf 5 (fun k->k "deps of %s/%s: %a" path m pp_strings l);
  l

(* find a topological ordering of given modules *)
let topo_order ~path ~deps modules : string list Lwt.t =
  let%lwt deps =
    Lwt_list.map_p
      (fun m ->
         find_deps ~deps ~path m
         >|= fun l -> m, List.filter (fun m' -> List.mem m' modules) l)
      modules
  in
  (* build a graph to obtain a topological order *)
  let g = CCGraph.make_tuple (fun m -> List.assoc m deps |> CCList.to_seq) in
  let l = CCGraph.topo_sort ~rev:true ~graph:g (CCList.to_seq modules) in
  Lwt.return l

(* build the .cmi for [m] *)
let build_interface ~path ~deps m : Maki.path Lwt.t =
  let file_mli = module_to_mli ~path m in
  let file_cmi = module_to_cmi ~path m in
  Maki.call_exn
    ~name:"build_interface"
    ~deps:[V.argv0; V.pack_file file_mli; V.pack_set V.string deps]
    ~op:V.file
    (fun () ->
       shellf "@[<h>ocamlfind ocamlc -c -I %s %a %s -o %s@]"
         path pp_strings_space (deps_to_args deps) file_mli file_cmi
       >>= fun (o,e,_) ->
       if CCIO.File.exists file_cmi
       then Lwt.return file_cmi
       else failf_lwt "failed to build interface %s of %s\n%s\n%s" file_cmi m o e)

(* build module [m] (after building its dependencies).
   @param path path in which [m] lives
   @param deps library [m] depends upon
   @param all_modules all modules in the current compilation unit *)
let rec build_module ~path ~deps ~all_mods m : Maki.path Lwt.t =
  (* compute deps *)
  let%lwt m_deps =
    find_deps ~deps ~path m
    >|= List.filter (fun m' -> List.mem m' all_mods)
  in
  (* build deps, obtain the resulting .cmo files *)
  let%lwt m_deps' =
    Lwt_list.map_p (fun m' -> build_module ~path ~deps ~all_mods m') m_deps
  in
  (* build interface *)
  let%lwt _ = build_interface ~path ~deps m in
  let file_ml = module_to_ml ~path m in
  let file_cmo = module_to_cmo ~path m in
  Maki.call_exn
    ~name:"build_module"
    ~deps:[V.argv0; V.pack_file file_ml;
           V.pack_set V.string deps; V.pack_set V.file m_deps']
    ~op:V.file
    (fun () ->
       shellf "@[<h>ocamlfind ocamlc -I %s -c %a %a %s -o %s@]"
         path
         pp_strings_space (deps_to_args deps)
         pp_strings_space m_deps'
         file_ml file_cmo
       >>= fun (o,e,_) ->
       if CCIO.File.exists file_cmo
       then Lwt.return file_cmo
       else failf_lwt "failed to build %s for %s:\n%s\n%s" file_cmo m o e)

let build_lib ~deps ~path ~name modules =
  (* build modules *)
  let%lwt () =
    Lwt_list.iter_p
      (fun m -> build_module ~deps ~path ~all_mods:modules m >|= fun _ -> ())
      modules
  in
  (* link in the proper order *)
  let%lwt l =
    topo_order ~path ~deps modules
    >|= List.map (module_to_cmo ~path)
  in
  let file_out = Filename.concat path (name ^ ".cma") in
  Maki.call_exn
    ~name:"build_lib"
    ~deps:[V.argv0; V.pack_list V.file l; V.pack_set V.string deps]
    ~op:V.file
    (fun () ->
       shellf "@[<h>ocamlfind ocamlc -a %a -o %s"
         pp_strings_space l file_out
       >>= fun (o,e,_) ->
       if CCIO.File.exists file_out then Lwt.return file_out
       else failf_lwt "error while building `%s` (out: %s, err:%s)" name o e
    )
  >|= fun _ -> ()

let build_exec ~deps ~path ~name:_ main_is : unit Lwt.t =
  (* get back to module name (so as to reuse find_deps) *)
  let main_m = Filename.chop_extension main_is |> String.capitalize in
  Maki_log.logf 5 (fun k->k "main module: %s (main_is: %s)" main_m main_is);
  (* find dependencies; only keep  those in the same path *)
  let%lwt m_deps =
    find_deps ~deps ~path main_m
    >|= List.filter
      (fun m -> try ignore (module_to_ml ~path m); true with _ -> false)
  in
  (* build deps *)
  let%lwt m_deps' =
    Lwt_list.map_p (build_module ~path ~deps ~all_mods:deps) m_deps
  in
  (* build main module *)
  let%lwt main' =
    build_module ~path ~deps ~all_mods:deps main_m
  in
  (* sort dependencies topologically *)
  let%lwt l =
    topo_order ~path ~deps m_deps
    >|= List.map (module_to_cmo ~path)
  in
  (* also depend on main module *)
  let l = l @ [main'] in
  let file_in = module_to_cmo ~path main_m in
  let file_out = Filename.concat path (set_ext ~ext:".byte" main_is) in
  Maki.call_exn
    ~name:"build_exec"
    ~deps:[V.argv0; V.pack_file file_in;
           V.pack_set V.string m_deps'; V.pack_set V.file l]
    ~op:V.file
    (fun () ->
       shellf "@[<h>ocamlfind ocamlc %a %a %s -o %s@]"
         pp_strings_space (deps_to_args deps)
         pp_strings_space l
         file_in file_out
       >>= fun (o,e,_) ->
       if CCIO.File.exists file_out
       then Lwt.return file_out
       else failf_lwt "failed to build binary `%s` for `%s`\n%s\n%s"
           file_out main_is o e
    )
  >|= fun _ -> ()

(** {2 Build following oasis} *)

(* find field "f: something" in the list [l] *)
let find_field ?or_ what f l =
  match
    CCList.find
      (function
        | A.S_field (f', A.F_set p) when f=f' -> Some p
        | _ -> None) l
  with
    | Some x -> x
    | None ->
      match or_ with
        | None -> failf "building `%s`: could not find \"%s\"" what f l
        | Some x -> x

let build_oasis_lib name l =
  let path = find_field name "Path" l |> String.concat "" in
  let modules = find_field name "Modules" l |> P.split_list in
  let deps = find_field name ~or_:[] "BuildDepends" l |> P.split_list in
  Maki_log.logf 2
    (fun k->k "build lib `%s`: path %s,@ @[modules `@[%a@]`@],@ @[depends `@[%a@]`@]"
        name path pp_strings modules pp_strings deps);
  build_lib ~deps ~path ~name modules

let build_oasis_exec name l  =
  let path = find_field name "Path" l |> String.concat "" in
  let main_is = find_field name "MainIs" l |> String.concat "" in
  let deps = find_field name ~or_:[] "BuildDepends" l |> P.split_list in
  Maki_log.logf 2
    (fun k->k "build executable `%s`: path %s,@ main_is: `%s`,@ @[depends `@[%a@]`@]"
        name path main_is pp_strings deps);
  build_exec ~deps ~path ~name main_is

let build_target stmts t =
  Maki_log.logf 1 (fun k->k "build target `%s`" t);
  let t' =
    CCList.find_map
      (function
        | A.TS_decl (A.Library, n, l) when n=t -> Some (`Lib l)
        | A.TS_decl (A.Executable, n, l) when n=t -> Some (`Exec l)
        | _ -> None)
      stmts
  in
  match t' with
    | Some (`Lib l) -> build_oasis_lib t l
    | Some (`Exec l) -> build_oasis_exec t l
    | None ->
      Maki_log.logf 0 (fun k->k "could not find target `%s`" t);
      exit 1

let build targets =
  let stmts = Oasis_parser.parse_file "_oasis" in
  Lwt_list.iter_s (build_target stmts) targets

(** {2 Main} *)

let () =
  let options =
    Arg.align
      [ "-d", Arg.Int Maki_log.set_level, " set debug level"
      ; "-j", Arg.Int Maki.Limit.set_j, " set parallelism level"
      ]
  in
  let l = ref [] in
  Arg.parse options
    (fun s -> l := s :: !l)
    "usage: maki_build [options] target [,target]*";
  Lwt_main.run (build (List.rev !l))
