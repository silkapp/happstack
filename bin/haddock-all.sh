#!/bin/sh

set -e

# package list
## required
PACKAGES="happstack-util happstack-data happstack-state happstack-ixset happstack-server happstack"
## optional
ghc-pkg field hamlet name          && PACKAGES="$PACKAGES happstack-hamlet"
ghc-pkg field hsp name             && PACKAGES="$PACKAGES happstack-hsp"
ghc-pkg field HStringTemplate name && PACKAGES="$PACKAGES happstack-hstringtemplate"
ghc-pkg field heist name           && PACKAGES="$PACKAGES happstack-heist"

# output directory
DESTDIR=haddock
mkdir -p $DESTDIR
REALDESTDIR=$(realpath $DESTDIR)
ROOTDIR=$(realpath .)

if "" == "$HTML_LOCATION" ; then
    HTML_LOCATION="$REALDESTDIR/doc/html/"
fi


echo "Haddock all happstack packages hyperlinkining source code"
for package in $PACKAGES
do
 cd $package
 cabal configure --builddir $REALDESTDIR && cabal haddock  --builddir $REALDESTDIR --hyperlink-source --internal --html-location="$HTML_LOCATION\$pkg"
 ARGS="$ARGS -i $package,$package/$package.haddock"
 cd ..
done



#ARGS="-i ../../happstack-util/dist/doc/html/happstack-util,happstack-util/dist/doc/html/happstack-util/happstack-util.haddock \
#   -i ../../happstack-data/dist/doc/html/happstack-data,happstack-data/dist/doc/html/happstack-data/happstack-data.haddock \
#   -i ../../happstack-state/dist/doc/html/happstack-state,happstack-state/dist/doc/html/happstack-state/happstack-state.haddock \
#   -i ../../happstack-ixset/dist/doc/html/happstack-ixset,happstack-ixset/dist/doc/html/happstack-ixset/happstack-ixset.haddock \
#   -i ../../happstack-server/dist/doc/html/happstack-server,happstack-server/dist/doc/html/happstack-server/happstack-server.haddock \
#   -i ../../happstack/dist/doc/html/happstack,happstack/dist/doc/html/happstack/happstack.haddock"

#ghc-pkg field hamlet name && ARGS="$ARGS -i ../../happstack-hamlet/dist/doc/html/happstack-hamlet,happstack-hamlet/dist/doc/html/happstack-hamlet/happstack-hamlet.#haddock"
#ghc-pkg field hsp name && ARGS="$ARGS   -i ../../happstack-hsp/dist/doc/html/happstack-hsp,happstack-hsp/dist/doc/html/happstack-hsp/happstack-hsp.haddock"
#ghc-pkg field HStringTemplate name && ARGS="$ARGS -i ../../happstack-hstringtemplate/dist/doc/html/happstack-hstringtemplate,happstack-hstringtemplate/dist/doc/html/happstack-hstringtemplate/happstack-hstringtemplate.haddock"

cd $REALDESTDIR/doc/html
haddock -t "Welcome to Happstack" -o .  --gen-contents --gen-index --prologue $ROOTDIR/haddock-prologue.txt  $ARGS
cd $ROOTDIR

echo "Done"
