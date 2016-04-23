{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  myemacs =
    with pkgs.emacsPackages; pkgs.emacsWithPackages
      [ haskellMode magit emacsw3m org ];
  myhaskell = pkgs.haskellPackages.ghcWithPackages (p: with p;
  [ base mtl cabal-install ]);
in with pkgs; stdenv.mkDerivation {
  name = "mtop";
  buildInputs = [ myemacs myhaskell ];
  shellHook = ''
    emacs-tcp PhilAlsa .emacs
  '';
}
