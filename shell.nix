{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:

let
  drv = (import ./default.nix) {
    inherit pkgs haskellPackages;
  };

in haskellPackages.shellFor {
  withHoogle = true;
  packages = _: [ drv ];
  buildInputs = (with pkgs; [
    cabal-install
    cabal2nix
    haskell-language-server
    hlint
    hpack
    python3
  ]) ++ (with haskellPackages; [
    ghcid
    ormolu
  ]);
}
