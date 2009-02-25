#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && cabal install "$@" && \
cd ../happstack-data    && cabal install "$@" && \
cd ../happstack-state   && cabal install "$@" && \
cd ../happstack-ixset   && cabal install "$@" && \
cd ../happstack-server  && cabal install "$@" && \
cd ../happstack-contrib && cabal install "$@" && \
cd ../happstack         && cabal install "$@" && \
cd .. && echo "Done"
