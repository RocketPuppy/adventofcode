{ pkgs ? import <nixpkgs-unstable> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.BNFC
    haskellPackages.alex
    haskellPackages.happy
    ghc
  ];
}
