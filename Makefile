
all: build

build:
	jbuilder build @install

test:
	jbuilder runtest --no-buffer --force

doc:
	jbuilder build @doc

clean:
	jbuilder clean

.PHONY: all build test clean doc

upload-doc: doc
	git checkout gh-pages && \
	  rm -rf dev/ && \
	  mkdir -p dev && \
	  cp -r maki.docdir/* dev/ && \
	  git add --all dev

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make all; \
	done
