{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:
haskellPackages.mkDerivation {
  pname = "nixon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = with haskellPackages; [
    aeson
    base
    bytestring
    containers
    directory
    foldl
    haskeline
    process
    text
    transformers
    turtle
    unix
    unordered-containers
    wordexp
  ];
  executableSystemDepends = with pkgs; [
    fzf
    rofi
  ];
  testDepends = with haskellPackages; [
    hspec
  ];
  license = pkgs.stdenv.lib.licenses.mit;
}
