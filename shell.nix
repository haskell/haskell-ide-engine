with import ./nixpkgs.nix {};
stdenv.mkDerivation {
  name = "haskell-ide-engine";
  buildInputs = [
    gmp
    zlib
    ncurses
    haskell.compiler.ghc843
    haskellPackages.cabal-install
  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
