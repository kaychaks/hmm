{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

in
{
  hmm = haskellPackages.developPackage {
    returnShellEnv = false;
    root = ./.;
  };
}
