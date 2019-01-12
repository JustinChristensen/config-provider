PROG := config-provider
TEST_PROG := config-provider-test

all: test

build: 
	cabal v2-build

test: 
	cabal v2-run $(TEST_PROG)

# use v2-test for this, because version 2.4.1.0 of cabal behaves oddly when using 
# it with v2-run
# also, cabal coverage doesn't seem to support text reports... how strange
coverage: 
	cabal v2-test $(TEST_PROG) --enable-coverage 

repl:
	cabal v2-repl

lint:
	hlint lint examples src test

clean:
	cabal v2-clean
	rm -rf dist dist-newstyle *.tix *.local *.local~ .hpc

dev-deps:
	cabal v2-install hlint

.PHONY: all build clean test repl lint coverage dev-deps