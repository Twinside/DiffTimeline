build:
	runhaskell Setup.hs build

conf:
	cabal-dev install

all:
	yesod --dev devel

# Do not use the normal folder version, sdist doesn't work
# on windows, instead, prepare a tiny tar gz and install
# it manually
add_private_package:
	cabal-dev add-source ../bidule.tar.gz
