#!/bin/sh
echo "Building and installing all cabal packages" && \
cd HAppS-Util       && cabal install -f tests "$@" && \
cd ../HAppS-Data    && cabal install -f tests "$@" && \
cd ../HAppS-State   && cabal install -f tests "$@" && \
cd ../HAppS-IxSet   && cabal install -f tests "$@" && \
cd ../HAppS-Server  && cabal install -f tests "$@" && \
cd ../HAppS-Contrib && cabal install -f tests "$@" && \
cd ../happstack-tests && cabal install -f tests "$@" && \
echo "Done"
