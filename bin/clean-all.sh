#!/bin/sh
echo "Creating source distribution tarballs in dist" && \
cd HAppS-Util       && cabal clean && \
cd ../HAppS-Data    && cabal clean && \
cd ../HAppS-State   && cabal clean && \
cd ../HAppS-IxSet   && cabal clean && \
cd ../HAppS-Server  && cabal clean && \
cd ../HAppS-Contrib && cabal clean && \
echo "Done"
