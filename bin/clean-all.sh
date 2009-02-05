#!/bin/sh
echo "Creating source distribution tarballs in dist" && \
cd happstack-util       && cabal clean && \
cd ../happstack-data    && cabal clean && \
cd ../happstack-state   && cabal clean && \
cd ../happstack-ixset   && cabal clean && \
cd ../happstack-server  && cabal clean && \
cd ../happstack-contrib && cabal clean && \
cd ../happstack-tests   && cabal clean && \
echo "Done"
