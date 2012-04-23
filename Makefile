build:
	runhaskell Setup.hs build

conf:
	cabal-dev install

all:
	yesod --dev devel

# Do not use the normal folder version, sdist doesn't work
# on windows, instead, prepare a tiny tar gz and install
# it manually
prepare:
	cd hit; make pack
	cabal-dev add-source hit/hit-0.3.tar.gz
	cabal-dev install

place:
	cp dist/build/DiffTimeline/DiffTimeline.exe ~/AppData/Roaming/cabal/bin/
