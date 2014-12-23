.PHONY: run

build: .cabal-sandbox/bin/wed-is-socket
	cabal install

run: build
	.cabal-sandbox/bin/wed-is-socket&
	.cabal-sandbox/bin/socket-client-test

