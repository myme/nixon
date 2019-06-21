{ pkgs ? import <nixpkgs> {} }:
with pkgs; with haskellPackages; callPackage ./derivation.nix {}
