#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && cabal install -f tests "$@" && \
cd ../happstack-data    && cabal install -f tests "$@" && \
cd ../happstack-state   && cabal install -f tests "$@" && \
cd ../happstack-ixset   && cabal install -f tests "$@" && \
cd ../happstack-server  && cabal install -f tests "$@" && \
cd ../happstack-hamlet  && cabal install -f tests "$@" && \
cd ../happstack         && cabal install -f tests "$@" && \
cd .. echo "Done"
