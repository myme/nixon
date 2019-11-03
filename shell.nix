{
  pkgs ? import <nixpkgs> {},
  haskellPackages ? pkgs.haskellPackages,
}:
let
  drv = (import ./default.nix) {
    inherit pkgs haskellPackages;
  };
in haskellPackages.shellFor {
  packages = _: [ drv ];
  buildInputs = (with pkgs; [
    cabal2nix
    cabal-install
    hlint
  ]) ++ (with haskellPackages; [
    ghcid
  ]);
}
