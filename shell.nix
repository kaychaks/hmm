{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", withHoogle ? false}:
let
  inherit (nixpkgs) pkgs;
  drv = import ./default.nix { inherit nixpkgs compiler withHoogle; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv [ pkgs.cabal-install ];
in
if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
