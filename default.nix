{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

in haskellPackages.mkDerivation {
  pname = "nixon";
  version = "0.1.0.0";
  src = (gitignore ./.);
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
    optparse-applicative
    quickcheck-instances
    process
    strip-ansi-escape
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
