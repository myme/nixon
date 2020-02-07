{
  pkgs ? (import ./nixpkgs.nix).nixpkgs { overlays = []; },
  haskellPackages ? pkgs.haskellPackages,
}:

let
  drv = (import ./default.nix) {
    inherit pkgs haskellPackages;
  };
  koi = (import ./nixpkgs.nix).koi { overlays = []; };

in haskellPackages.shellFor {
  packages = _: [ drv ];
  buildInputs = (with pkgs; [
    cabal2nix
    hlint
  ]) ++ (with koi; [
    cabal-install
  ]) ++ (with haskellPackages; [
    ghcid
  ]);
}
