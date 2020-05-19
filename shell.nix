{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:

let
  drv = (import ./default.nix) {
    inherit pkgs haskellPackages;
  };

in haskellPackages.shellFor {
  packages = _: [ drv ];
  buildInputs = (with pkgs; [
    cabal-install
    cabal2nix
    hlint
  ]) ++ (with haskellPackages; [
    ghcid
  ]);
}
