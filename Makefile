.PHONY: run

build: .cabal-sandbox/bin/wed-is-socket
	cabal install -j
	cabal freeze
