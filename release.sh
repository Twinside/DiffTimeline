#!/bin/sh

# Configuration
OS=`uname`
VERSION="1.0"

# First little cleanup
rm -Rf cabal-dev
rm -Rf dist

# Then make sure we are up to date
git pull
git submodule update

cd hit-simple
rm *.gz
make pack
cd ..

cd ClosureExternalProducer
rm *.gz
make pack
cd ..

# make sure cabal-dev is here
export PATH=$HOME/.cabal/bin:$PATH

# append files
cabal-dev add-source hit-simple/hit-simple-0.3.tar.gz
cabal-dev add-source ClosureExternalProducer/ClosureExternalProducer-0.1.tar.gz
cabal-dev install

# Prepare
cp dist/build/difftimeline/difftimeline .
strip difftimeline
binaryfile="difftimeline-$VERSION-$OS.tar.bz2"
tar cvjf $binaryfile difftimeline

# push to intarweb
git branch -D gh-pages
git co -b gh-pages origin/gh-pages
mv $binaryfile binaries/
git add "binaries/$binaryfile"
git commit -m "New binary for $OS (version : $VERSION)"
git push
git co master

