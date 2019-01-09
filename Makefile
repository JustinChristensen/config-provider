all: test

build: 
	cabal v2-build

test: 
	cabal v2-run config-provider-test

clean:
	cabal v2-clean
	rm -rf dist dist-newstyle

.PHONY: all build clean test