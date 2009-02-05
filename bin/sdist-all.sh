#!/bin/sh
echo "Creating source distribution tarballs in dist" && \
cd happstack-util       && cabal sdist --builddir=../dist && \
cd ../happstack-data    && cabal sdist --builddir=../dist && \
cd ../happstack-state   && cabal sdist --builddir=../dist && \
cd ../happstack-ixset   && cabal sdist --builddir=../dist && \
cd ../happstack-server  && cabal sdist --builddir=../dist && \
cd ../happstack-contrib && cabal sdist --builddir=../dist && \
echo "Done"
