all: test

build: 
	cabal v2-build

test: 
	cabal v2-run config-provider-test

repl:
	cabal v2-repl

lint:
	hlint lint examples src test

clean:
	cabal v2-clean
	rm -rf dist dist-newstyle

.PHONY: all build clean test repl lint