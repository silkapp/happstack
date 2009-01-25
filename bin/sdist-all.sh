#!/bin/sh
echo "Creating source distribution tarballs in dist" && \
cd HAppS-Util      && cabal sdist --builddir=../dist && \
cd ../HAppS-Data   && cabal sdist --builddir=../dist && \
cd ../HAppS-State  && cabal sdist --builddir=../dist && \
cd ../HAppS-IxSet  && cabal sdist --builddir=../dist && \
cd ../HAppS-Server && cabal sdist --builddir=../dist && \
echo "Done"
