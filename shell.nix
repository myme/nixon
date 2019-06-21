{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  drv = pkgs.haskellPackages.callPackage ./derivation.nix {
    buildTools = with pkgs; [
      cabal2nix
      cabal-install
    ];
  };
in drv.env
