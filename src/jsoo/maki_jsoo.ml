open Maki.E

let make (module S : WebStorage.STORAGE) name : Maki.Storage.t = {
  name;
  get = (fun key -> Lwt.return_ok (S.get key));
  set = (fun key value -> Lwt.return_ok (S.set key value));
  remove = (fun key -> Lwt.return (S.remove key));
  fold = (fun ~f ~x ->
    let acc = ref @@ return x in
    S.iter (fun key value ->
      acc := !acc >>= fun x -> f x (key, value)
    );
    !acc
  );
  flush_cache = S.clear;
}

let local_storage = make (module WebStorage.Local) "localStorage"
let session_storage = make (module WebStorage.Session) "sessionStorage"
