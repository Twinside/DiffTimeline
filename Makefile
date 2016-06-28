ifeq ($(shell uname),WindowsNT)
	SHELL:=cmd
else
endif

ifeq ($(shell uname),Darwin)
	CABAL_FLAG:=-f Dynlink
else
	CABAL_FLAG:=
endif

all: static-content/third_party.js composed.js static-content/third_party.js Difftimeline/Foundation.hs
	stack build

build: static-content/third_party.js composed.js static-content/third_party.js Difftimeline/Foundation.hs
	cabal build

pre: composed.js static-content/third_party.js Difftimeline/Foundation.hs

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

hlint:
	hlint .

# Do not use the normal folder version, sdist doesn't work
# on windows, instead, prepare a tiny tar gz and install
# it manually
prepare:
	cabal sandbox init
	cabal sandbox add-source hit-simple
	cabal sandbox add-source ClosureExternalProducer
	make composed.js
	make static-content/third_party.js
	cabal install $(CABAL_FLAG)

place:
	cp dist/build/DiffTimeline/DiffTimeline.exe ~/AppData/Roaming/cabal/bin/

unixplace:
	cp dist/build/difftimeline/difftimeline ~/.cabal/bin/


#--compilation_level ADVANCED_OPTIMIZATIONS \

JS_FILE_NAMES:= \
	blameshower.ts \
	branch_list.ts \
	breadcrumb.ts \
	commit.ts \
	commitcomparer.ts \
	commitrenderer.ts \
	constants.ts \
	diffmanipulator.ts \
	difftimeline.extern.ts \
	fileblob.ts \
	filecomparer.ts \
	filerenderer.ts \
	global_constant.ts \
	init.ts \
	jquery.d.ts \
	keybindings.ts \
	linealign.ts \
	project.ts \
	resultset.ts \

JS_FILES:=$(addprefix static-content/frontend/,$(JS_FILE_NAMES))
JS_THIRD_PARTY_FILES:=$(addprefix static-content/,$(JS_THIRD_PARTY_NAMES))

composed.js: $(JS_FILES)
	cd static-content; tsc

JS_THIRD_PARTY_NAMES:= \
	jquery-1.9.1.min.js \
	jquery-ui-1.10.2.custom.min.js \
	jquery-hotkeys.js \
	jquery-scrollTo.js \
	ICanHaz.min.js


static-content/third_party.js: $(JS_THIRD_PARTY_FILES)
	cat $(JS_THIRD_PARTY_FILES) > $@

