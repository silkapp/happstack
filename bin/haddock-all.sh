#!/bin/sh
echo "Haddock all happstack packages hyperlinkining source code"
cd happstack-util       && cabal configure && cabal haddock --hyperlink-source --internal
cd ../happstack-data    && cabal configure && cabal haddock --hyperlink-source --internal
cd ../happstack-state   && cabal configure && cabal haddock --hyperlink-source --internal
cd ../happstack-ixset   && cabal configure && cabal haddock --hyperlink-source --internal
cd ../happstack-server  && cabal configure && cabal haddock --hyperlink-source --internal
cd ../happstack         && cabal configure && cabal haddock --hyperlink-source --internal
cd ..

ARGS="-i ../../happstack-util/dist/doc/html/happstack-util,happstack-util/dist/doc/html/happstack-util/happstack-util.haddock \
   -i ../../happstack-data/dist/doc/html/happstack-data,happstack-data/dist/doc/html/happstack-data/happstack-data.haddock \
   -i ../../happstack-state/dist/doc/html/happstack-state,happstack-state/dist/doc/html/happstack-state/happstack-state.haddock \
   -i ../../happstack-ixset/dist/doc/html/happstack-ixset,happstack-ixset/dist/doc/html/happstack-ixset/happstack-ixset.haddock \
   -i ../../happstack-server/dist/doc/html/happstack-server,happstack-server/dist/doc/html/happstack-server/happstack-server.haddock \
   -i ../../happstack/dist/doc/html/happstack,happstack/dist/doc/html/happstack/happstack.haddock"

haddock -t "Welcome to Happstack" -o doc/html --gen-contents --gen-index  $ARGS 

echo "Done"
