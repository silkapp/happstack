#!/bin/sh
echo "unregistering all cabal packages" && \
ghc-pkg unregister happstack-hamlet --user
ghc-pkg unregister happstack-wai --user
ghc-pkg unregister happstack --user
ghc-pkg unregister happstack-server --user
ghc-pkg unregister happstack-state --user
ghc-pkg unregister happstack-ixset --user
ghc-pkg unregister happstack-data --user
ghc-pkg unregister happstack-util --user
cd .. && echo "Done"
