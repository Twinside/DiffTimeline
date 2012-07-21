

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
 * Clone the repository
 * `git submodule init && git submodule update`
 * Install the latest [Haskell Platform](http://hackage.haskell.org/platform/ Haskell Platform download link)
 * Install the cabal-dev `cabal install cabal-dev` utility
 * In the main folder call `make prepare`
   * require zlib & gmp static libs 
 * After that, you can build calling just make

And that should be it

Running
-------
to launch `difftimeline <filename>` or `difftimeline` or `difftimeline compare`

--
Gentoo practical example (on amd64)
===================================
Warning : this information might be outdated

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

