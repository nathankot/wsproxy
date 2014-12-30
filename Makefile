.PHONY: run

build: .cabal-sandbox/bin/wed-is-socket
	cabal freeze
	cabal install -j

test:
	cabal install -j --enable-tests
	cabal run tests

run: build
	cabal run wxproxy

