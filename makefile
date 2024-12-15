PACKAGES = regex-pcre-0.95.0.0


run:
	cabal run +RTS -N16 -RTS
clean:
	cabal clean

build:
	cabal build

