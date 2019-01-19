
build:
	@dune build @install

dev: test

test:
	@dune runtest --no-buffer --force

doc:
	@dune build @doc

clean:
	@dune clean

reindent:
	find -name '*.ml*' -exec ocp-indent -i {} \;

.PHONY: all build test clean doc

watch:
	@dune build -w
