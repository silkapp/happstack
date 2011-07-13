#!/bin/sh

#
# To build for uploading to happstack.com do:
#
#  HTML_LOCATION=http://www.happstack.com/docs/6.0.0/ ./bin/haddock-all.sh
# 

set -e

# package list
## required
PACKAGES="happstack-util happstack-data happstack-state ixset happstack-ixset happstack-server happstack"
## optional
ghc-pkg field hamlet name          && PACKAGES="$PACKAGES happstack-hamlet"
ghc-pkg field hsp name             && PACKAGES="$PACKAGES happstack-hsp"
ghc-pkg field HStringTemplate name && PACKAGES="$PACKAGES happstack-hstringtemplate"
ghc-pkg field heist name           && PACKAGES="$PACKAGES happstack-heist"
ghc-pkg field jmacro name          && PACKAGES="$PACKAGES happstack-jmacro"
ghc-pkg field jmacro name && ghc-pkg field hsx name && PACKAGES="$PACKAGES hsx-jmacro"

# output directory
DESTDIR=$(pwd)/haddock
mkdir -p $DESTDIR
ROOTDIR=$(pwd)

if [ "" = "$HTML_LOCATION" ] ; then
    HTML_LOCATION="$DESTDIR/doc/html/"
fi

echo "Haddock all happstack packages hyperlinkining source code"
for package in $PACKAGES
do
 cd $package
 cabal configure --builddir $DESTDIR && cabal haddock  --builddir $DESTDIR --hyperlink-source --internal --html-location="$HTML_LOCATION\$pkg"
 ARGS="$ARGS -i $package,$package/$package.haddock"
 cd ..
done

cd $DESTDIR/doc/html
haddock -t "Welcome to Happstack" -o .  --gen-contents --gen-index --prologue $ROOTDIR/haddock-prologue.txt  $ARGS
cd $ROOTDIR

echo "Done."
echo "$DESTDIR/doc/html/index.html"
