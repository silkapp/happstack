#!/usr/bin/env sh

cabal configure &&
cabal build -v0 &&
runghc -isrc -itests tests/RunTests.hs || exit 1


for f in `find Examples/ -name '*.hs'`; do
    touch $f;
    ghc -v0 -isrc --make -c $f || exit 1;
    echo -n .;
 done
echo Done
