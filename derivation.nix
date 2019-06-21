{ mkDerivation, base, directory, foldl, stdenv, turtle, wordexp, buildTools ? [] }:
mkDerivation {
  pname = "envix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base directory foldl turtle wordexp ];
  license = stdenv.lib.licenses.mit;
  inherit buildTools;
}
