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
* Mae sure you got static zlib & gmp (as root)
   * `echo 'sys-libs/zlib' >> /etc/portage/package.use`
   * `echo 'dev-libs/gmp static-libs' >> /etc/portage/package.use`
   * `emerge -DuNa @world`
* In the main folder call `make prepare` (as user)