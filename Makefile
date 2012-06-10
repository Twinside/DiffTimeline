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
	cd hit-simple; make pack
	cabal-dev add-source hit-simple/hit-simple-0.3.tar.gz
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
		 --summary_detail_level 3 \
		 --js static-content/tinysyntaxhighlighter.js

meh:
		 --js static-content/difftimeline.js \
