#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && cabal install -f tests "$@" && \
cd ../happstack-data    && cabal install -f tests "$@" && \
cd ../happstack-state   && cabal install -f tests "$@" && \
cd ../happstack-ixset   && cabal install -f tests "$@" && \
cd ../happstack-server  && cabal install -f tests "$@" && \
cd ../happstack         && cabal install -f tests "$@" && \
cd ../happstack-hamlet  && if ghc-pkg field hamlet name    > /dev/null 2> /dev/null ; then cabal install -f tests "$@"  ; else echo "happstack-hamlet skipped because hamlet is not installed." ; fi && \
cd ../happstack-wai     && if ghc-pkg field wai-extra name > /dev/null 2> /dev/null ; then cabal install -f tests "$@"  ; else echo "happstack-wai skipped because wai-extra is not installed." ; fi && \
cd .. echo "Done"
