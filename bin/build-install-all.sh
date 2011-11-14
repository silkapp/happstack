#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && cabal install "$@" && \
cd ../happstack-data    && cabal install "$@" && \
cd ../happstack-state   && cabal install "$@" && \
cd ../happstack-ixset   && cabal install "$@" && \
cd ../ixset             && cabal install "$@" && \
cd ../happstack-server  && cabal install "$@" && \
cd ../happstack         && cabal install "$@" && \
cd ../happstack-lite    && cabal install "$@" && \
cd ../happstack-hsp     && if ghc-pkg field hsp name       > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-hsp skipped because hsp is not installed." ; fi && \
cd ../happstack-hstringtemplate     && if ghc-pkg field HStringTemplate name       > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-hstringtemplate skipped because HStringTemplate is not installed." ; fi && \
cd ../happstack-hamlet  && if ghc-pkg field hamlet name    > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-hamlet skipped because hamlet is not installed." ; fi && \
cd ../happstack-wai     && if ghc-pkg field wai-extra name > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-wai skipped because wai-extra is not installed." ; fi && \
cd ../happstack-heist   && if ghc-pkg field heist name     > /dev/null 2> /dev/null ; then cabal install "$@"  ; else echo "happstack-heist skipped because heist is not installed." ; fi && \
cd .. && echo "Done"
