#!/bin/sh
echo "unregistering all cabal packages" && \
ghc-pkg unregister happstack-hstringtemplate --user
ghc-pkg unregister happstack-hsp     --user
ghc-pkg unregister happstack-heist   --user
ghc-pkg unregister happstack-hamlet  --user
ghc-pkg unregister happstack-plugins --user
ghc-pkg unregister happstack-wai     --user
ghc-pkg unregister happstack         --user
ghc-pkg unregister happstack-server  --user
ghc-pkg unregister happstack-state   --user
ghc-pkg unregister happstack-ixset   --user
ghc-pkg unregister happstack-data    --user
ghc-pkg unregister happstack-util    --user
cd .. && echo "Done"
