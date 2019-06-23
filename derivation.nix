{
  mkDerivation,
  stdenv,
  base, directory, foldl, text, turtle, wordexp,
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
    text
    turtle
    wordexp
  ];
  executableSystemDepends = [
    rofi
  ];
  license = stdenv.lib.licenses.mit;
  inherit buildTools;
}
