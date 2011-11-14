#!/bin/sh

for package in happstack-util happstack-data ixset happstack-ixset happstack-state happstack-server happstack happstack-hamlet happstack-heist happstack-hsp happstack-hstringtemplate happstack-plugins happstack-lite
do
  packdeps $package/$package.cabal
done

