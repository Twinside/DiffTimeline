

How to hack without an Haskell Compiler
=======================================
Difftimeline being really splited in two parts, the haskell server and the
"web" frontend, you can hack the web frontend without any compiler installed.
In order to do so, you must :

 * Grab a Difftimeline binary for your platform in the download section
 * Copy the static-content folder of the repository into one of your folder
   say `~/.config/difftimeline/static-content`
 * Invoque difftimeline with a new flag `difftimeline -d ~/.config/difftimeline/static-content`
   This way, the files in your personnal folder will be served instead of the embedded one

Happy hacking!

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
   * require zlib & gmp static libs 
 * After that, you can build calling just make

And that should be it

Running
-------
to launch `difftimeline <filename>`

--
Gentoo practical example (on amd64)
========================
* Install GHC 7.4 and `cabal-install` (as root)
   * `echo '=dev-lang/ghc-7.4.1 ~amd64' >> /etc/portage/package.accept_keywords`
   * `echo '=dev-haskell/network-2.3.0.11 ~amd64' >> /etc/portage/package.accept_keywords`
   * `emerge cabal-install`
* Install the cabal-dev (as user)
   * `cabal install cabal-dev`
* Make sure you got static zlib & gmp (as root)
   * `echo 'sys-libs/zlib static-libs' >> /etc/portage/package.use`
   * `echo 'dev-libs/gmp static-libs' >> /etc/portage/package.use`
   * `emerge -DuNa @world`
* In the main folder call `make prepare` (as user)
