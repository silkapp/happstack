#!/bin/sh
echo "Building and installing all cabal packages" && \
cd HAppS-Util       && cabal install "$@" && \
cd ../HAppS-Data    && cabal install "$@" && \
cd ../HAppS-State   && cabal install "$@" && \
cd ../HAppS-IxSet   && cabal install "$@" && \
cd ../HAppS-Server  && cabal install "$@" && \
cd ../HAppS-Contrib && cabal install "$@" && \
echo "Done"
