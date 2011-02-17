#!/bin/sh
echo "Creating source distribution tarballs in dist"

for package in happstack-util happstack-data happstack-ixset happstack-state happstack-server happstack happstack-hamlet happstack-heist happstack-hsp happstack-hstringtemplate happstack-plugins
do
    cd $package
    echo "checking $package..."
    cabal check
    cd ..

done

echo "check-all.sh done."