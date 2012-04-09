mup:
	runhaskell -Wall hsDiffTest.hs

moh:
	ghc -c -Wall DiffTimeline/Diff.hs -package vector
	# ghc -c -Wall DiffTimeline/Diff.hs DiffTimeline/GitQuery.hs -package vector

check:
	java -jar compiler.jar \
			--warning_level=verbose \
			--js static-content/difftimeline.js \
			--js static-content/tinysyntaxhighlighter.js \

meh:
			--externs static-content/jquery-1.6.4.min.js \
			--externs static-content/underscore-min.js \
			--externs static-content/ICanHaz.min.js

help:
	java -jar compiler.jar --help


pack:
	tar cvf DiffTimeline.tar difftimeline.rb lib stat-content vendor Gemfile Gemfile.lock README.md
	gzip DiffTimeline.tar
