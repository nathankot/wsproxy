.PHONY: run

make: .cabal-sandbox/bin/wed-is-socket
	cabal install --only-dependencies -j
	cabal freeze
