version=$(shell grep '^Version:' scholdoc.cabal | awk '{print $$2;}')
setup=dist/setup/setup

quick:
	cabal configure --enable-tests --disable-optimization
	cabal build

full:
	cabal configure --enable-tests --enable-optimization -ftryscholdoc -fembed_data_files --enable-benchmarks
	cabal build
	cabal haddock

deps:
	cabal install --only-dependencies --enable-tests -ftryscholdoc -fembed_data_files --enable-benchmarks

prof:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-optimization --enable-tests
	cabal build

test:
	cabal test

bench:
	cabal bench

install: full
	cabal copy
	cabal register

dist:
	cabal sdist
	rm -rf "scholdoc-${version}"
	tar xvzf dist/scholdoc-${version}.tar.gz
	cd scholdoc-${version}
	cabal configure ${CABALARGS} && cabal build && cabal test && cd .. && rm -rf "scholdoc-${version}"

osxpkg:
	./make_osx_package.sh

clean:
	cabal clean

.PHONY: deps quick full install clean test bench haddock osxpkg dist prof
