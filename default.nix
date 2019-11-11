{
  pkgs ? import <nixpkgs> {},
  haskellPackages ? pkgs.haskellPackages,
}:
haskellPackages.mkDerivation {
  pname = "envix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = with haskellPackages; [
    base
    containers
    directory
    foldl
    haskeline
    text
    turtle
    unix
    wordexp
  ];
  executableSystemDepends = with pkgs; [
    fzf
    rofi
  ];
  license = pkgs.stdenv.lib.licenses.mit;
}
