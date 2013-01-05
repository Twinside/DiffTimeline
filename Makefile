ifeq ($(shell uname),WindowsNT)
	SHELL:=cmd
else
endif

all: build

build: composed.js Difftimeline/Foundation.hs
	runhaskell Setup.hs build

# Modify the file date to let the build system detect
# modification and reparse the routes using template
# haskell.
Difftimeline/Foundation.hs: config/routes
	touch Difftimeline/Foundation.hs

blame_tests:
	runhaskell -package-conf=cabal-dev/packages-7.4.1.conf -cpp -Wall test/blame_test.hs

tests:
	runhaskell -package-conf=cabal-dev/packages-7.4.1.conf test/tester.hs

ghci:
	ghci -package-conf=cabal-dev/packages-7.4.1.conf

conf:
	cabal-dev install

all:
	yesod --dev devel

hlint:
	hlint .

# Do not use the normal folder version, sdist doesn't work
# on windows, instead, prepare a tiny tar gz and install
# it manually
prepare:
	cd hit-simple; make pack
	cabal-dev add-source hit-simple/hit-simple-0.3.tar.gz
	cd ClosureExternalProducer; make pack
	cabal-dev add-source ClosureExternalProducer/ClosureExternalProducer-0.1.tar.gz
	cabal-dev install

place:
	cp dist/build/DiffTimeline/DiffTimeline.exe ~/AppData/Roaming/cabal/bin/

unixplace:
	cp dist/build/difftimeline/difftimeline ~/.cabal/bin/

composed.js: static-content/difftimeline.js static-content/tinysyntaxhighlighter.js
	java -jar compiler.jar \
		 --warning_level VERBOSE \
		 --js_output_file composed.js \
		 --jscomp_warning=checkTypes \
		 --externs test/externs/jquery-1.7.js \
		 --externs test/externs/jquery.ext.js \
		 --externs test/externs/icanhaz.extern.js \
		 --externs test/externs/difftimeline.extern.js \
		 --summary_detail_level 3 \
		 --js static-content/difftimeline.js \
		 --js static-content/tinysyntaxhighlighter.js

#--compilation_level ADVANCED_OPTIMIZATIONS \

JS_FILE_NAMES:= \
	global_constants.js \
	breadcrumb.js \
	resultset.js \
	project.js \
	diffmanipulator.js \
	commit.js \
	commitrenderer.js \
	fileblob.js \
	filerenderer.js \
	commitcomparer.js \
	filecomparer.js \
	blameshower.js \
	init.js \
	branch_list.js \
	keybindings.js

JS_FILES:=$(addprefix static-content/frontend/,$(JS_FILE_NAMES))

static-content/difftimeline.js: $(JS_FILES)
	cat $(JS_FILES) > $@

