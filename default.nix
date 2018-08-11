{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  myemacs =
    with pkgs.emacsPackages; with pkgs.emacsPackagesNg; pkgs.emacsWithPackages
      [ haskellMode magit ];
  myhaskell = pkgs.haskellPackages.ghcWithPackages (p: with p;
  [ base mtl cabal-install ]);
in with pkgs; stdenv.mkDerivation {
  name = "PhiledAlsa";
  buildInputs = [ myemacs myhaskell alsaLib ];
  shellHook = ''
    emacs-tcp PhilAlsa .emacs
  '';
  LD_LIBRARY_PATH="${pkgs.alsaLib}/lib";
}
