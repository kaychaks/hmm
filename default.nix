{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", withHoogle ? false }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

  hPkgs = if withHoogle
  then
    haskellPackages // rec {
      ghc = haskellPackages.ghc // { withPackages = haskellPackages.ghc.withHoogle; };
      ghcWithPackages = ghc.withPackages;
    }
  else
    haskellPackages;

in
hPkgs.callPackage ./hmm.nix {}
