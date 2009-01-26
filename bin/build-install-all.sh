#!/bin/sh
echo "Building and installing all cabal packages" && \
cd HAppS-Util      && cabal configure && cabal build && cabal install && \
cd ../HAppS-Data   && cabal configure && cabal build && cabal install && \
cd ../HAppS-State  && cabal configure && cabal build && cabal install && \
cd ../HAppS-IxSet  && cabal configure && cabal build && cabal install && \
cd ../HAppS-Server && cabal configure && cabal build && cabal install && \
cd ../HAppS-DNS    && cabal configure && cabal build && cabal install && \
echo "Done"
