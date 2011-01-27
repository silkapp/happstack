#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install && \
cd ../happstack-data    && runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install && \
cd ../happstack-state   && runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install && \
cd ../happstack-ixset   && runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install && \
cd ../happstack-server  && runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install && \
cd ../happstack         && runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install && \
cd ../happstack-hsp     && if ghc-pkg field hsp name   > /dev/null 2> /dev/null ; then runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install ; else echo "happstack-hsp skipped because hsp is not installed." ; fi && \
cd ../happstack-hstringtemplate     && if ghc-pkg field HStringTemplate name   > /dev/null 2> /dev/null ; then runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install ; else echo "happstack-hstringtemplate skipped because HStringTemplate is not installed." ; fi && \
cd ../happstack-heist   && if ghc-pkg field heist name > /dev/null 2> /dev/null ; then runhaskell Setup configure --user "$@" && runhaskell Setup build && runhaskell Setup install ; else echo "happstack-heist skipped because heist is not installed." ; fi && \
cd .. echo "Done"
