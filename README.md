How to build Difftimeline
===========================
 * Install the `hit` sub module, calling 
   `git submodule init` and `git submodule update`
 * Install GHC 7.4 (haskell compiler), and find
   a way to install `cabal-install`, under windows
   you should use mingw to build it (only way to 
   build the Network package)

 * Install the cabal-dev `cabal install cabal-dev`
 * In the main folder call `make prepare`
 * After that, you can build calling just make

And that should be it

Running
-------
to launch `difftimeline <filename>`

