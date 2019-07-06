{
  pkgs ? import <nixpkgs> {},
  haskellPackages ? pkgs.haskellPackages,
}:
let
  drv = haskellPackages.mkDerivation {
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
        process
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
    };
in if !pkgs.lib.inNixShell
  then
    drv
  else
    haskellPackages.shellFor {
      packages = _: [ drv ];
      buildInputs = (with pkgs; [
        cabal2nix
        cabal-install
        hlint
      ]) ++ (with haskellPackages; [
        ghcid
      ]);
    }
