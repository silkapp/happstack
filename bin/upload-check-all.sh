#!/bin/sh
echo "Building and installing all cabal packages" && \
cd happstack-util       && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-data    && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-state   && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-ixset   && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-server  && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack         && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-hamlet  && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-heist   && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-hsp     && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd ../happstack-hstringtemplate && cabal sdist && cabal upload --verbose=3 --check dist/*.tar.gz && \
cd .. && echo "All checked out OK"
