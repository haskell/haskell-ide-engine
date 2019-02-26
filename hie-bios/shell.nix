with { pkgs = import ./nix {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.niv pkgs.haskell.compiler.ghc863 pkgs.haskell.packages.ghc863.cabal-install ];
  }
