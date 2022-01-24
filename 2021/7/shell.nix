{ pkgs ? import <nixos> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [megaparsec text]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.BNFC
    haskellPackages.alex
    haskellPackages.happy
    ghc
  ];
}
