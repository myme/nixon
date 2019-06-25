{
  mkDerivation,
  stdenv,
  base, directory, foldl, process, text, turtle, unix, wordexp,
  fzf,
  rofi,
  buildTools ? []
}:
mkDerivation {
  pname = "envix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    directory
    foldl
    process
    text
    turtle
    unix
    wordexp
  ];
  executableSystemDepends = [
    fzf
    rofi
  ];
  license = stdenv.lib.licenses.mit;
  inherit buildTools;
}
