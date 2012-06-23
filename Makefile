all: build

build:
	runhaskell Setup.hs build

clos:
	ghc -package-conf=cabal-dev/packages-7.4.1.conf --make .\clos_test.hs

conf:
	cabal-dev install

all:
	yesod --dev devel

# Do not use the normal folder version, sdist doesn't work
# on windows, instead, prepare a tiny tar gz and install
# it manually
prepare:
	cd hit-simple; make pack
	cabal-dev add-source hit-simple/hit-simple-0.3.tar.gz
	cd ClosureExternalProducer; make pack
	cabal-dev add-source ClosureExternalProducer/ClosureExternalProducer-0.1.tar.gz
	cabal-dev install

	cabal-dev install

place:
	cp dist/build/DiffTimeline/DiffTimeline.exe ~/AppData/Roaming/cabal/bin/

check:
	java -jar compiler.jar \
		 --warning_level VERBOSE \
		 --js_output_file composed.js \
		 --jscomp_warning=checkTypes \
		 --externs jquery-1.6.js \
		 --externs icanhaz.extern.js \
		 --externs difftimeline.extern.js \
		 --summary_detail_level 3 \
		 --js static-content/difftimeline.js \
		 --js static-content/tinysyntaxhighlighter.js

