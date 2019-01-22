PROG := config-provider
TEST_PROG := config-provider-test
EX_SCOTTY := examples/scotty

all: build

build: 
	cabal v2-build

scotty-ex: 
	cd $(EX_SCOTTY) && cabal v2-build

run: 
	cabal v2-run config-provider

run-scotty-ex: 
	cd $(EX_SCOTTY) && cabal v2-run $(PROG)-scotty-example

test: 
	cabal v2-run $(TEST_PROG) --enable-tests

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

.PHONY: \
all \
build \
clean \
test \
repl \
lint \
coverage \
dev-deps \
run \
scotty-ex \
run-scotty-ex