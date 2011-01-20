#!/bin/sh
echo "Cleaning all packages" && \
cd happstack-util       && cabal clean && \
cd ../happstack-data    && cabal clean && \
cd ../happstack-state   && cabal clean && \
cd ../happstack-ixset   && cabal clean && \
cd ../happstack-server  && cabal clean && \
cd ../happstack-hsp     && cabal clean && \
cd ../happstack-hamlet  && cabal clean && \
cd ../happstack-wai     && cabal clean && \
cd ../happstack-heist   && cabal clean && \
cd ../happstack         && cabal clean && \
cd .. && echo "Done"
