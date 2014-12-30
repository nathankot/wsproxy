.PHONY: run

build: .cabal-sandbox/bin/wed-is-socket
	cabal freeze
	cabal install -j

run: build
	.cabal-sandbox/bin/wed-is-socket&
	.cabal-sandbox/bin/socket-client-test

