#!/bin/sh
echo "Creating source distribution tarballs in dist" && \
cd happstack-util       && cabal sdist --builddir=../dist && \
cd ../happstack-data    && cabal sdist --builddir=../dist && \
cd ../happstack-state   && cabal sdist --builddir=../dist && \
cd ../happstack-ixset   && cabal sdist --builddir=../dist && \
cd ../happstack-server  && cabal sdist --builddir=../dist && \
cd ../happstack-hamlet  && cabal sdist --builddir=../dist && \
cd ../happstack-heist   && cabal sdist --builddir=../dist && \
cd ../happstack-hsp     && cabal sdist --builddir=../dist && \
cd ../happstack-hstringtemplate && cabal sdist --builddir=../dist && \
cd ../happstack         && cabal sdist --builddir=../dist && \
cd .. && echo "Done"
