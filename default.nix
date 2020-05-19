{
  pkgs ? import ./nixpkgs.nix {},
  haskellPackages ? pkgs.haskellPackages,
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  src = gitignore ./.;

in haskellPackages.mkDerivation {
  inherit src;
  pname = "nixon";
  version = "0.1.0.0";
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
  postInstall = ''
    mkdir -p $out/share/zsh/site-functions
    install ${src}/extra/nixon-widget.zsh $out/share/zsh/site-functions/_nixon_widget
  '';
}
