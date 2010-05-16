#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && cabal install "$@" && \
cd ../happstack-data    && cabal install "$@" && \
cd ../happstack-state   && cabal install "$@" && \
cd ../happstack-ixset   && cabal install "$@" && \
cd ../happstack-server  && cabal install "$@" && \
cd ../happstack         && cabal install "$@" && \
cd ../happstack-hamlet  && if ghc-pkg field hamlet name    > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-hamlet skipped because hamlet is not installed." ; fi && \
cd ../happstack-wai     && if ghc-pkg field wai-extra name > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-wai skipped because wai-extra is not installed." ; fi && \
cd .. && echo "Done"
