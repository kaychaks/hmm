{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", withHoogle ? false}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

  project = import ./. { inherit nixpkgs compiler; };
in
haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [ project.hmm ];
}
