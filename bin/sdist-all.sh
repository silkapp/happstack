#!/bin/sh
echo "Creating source distribution tarballs in dist"

for package in happstack-util happstack-data ixset happstack-ixset happstack-state happstack-server happstack happstack-hamlet happstack-heist happstack-hsp happstack-hstringtemplate happstack-plugins
do
    cd $package
    cabal configure --builddir=../sdist
    cabal sdist     --builddir=../sdist
    cd ..

done

echo "sdist-all.sh done."