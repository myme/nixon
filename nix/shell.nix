{
  pkgs,
  craneLib,
  toolchain,
  guiRuntimeLibraries,
  guiRuntimePrograms,
}:

craneLib.devShell {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath guiRuntimeLibraries;

  packages = [
    toolchain
  ]
  ++ guiRuntimePrograms
  ++ (with pkgs; [
    # Dev loop and test tooling.
    bacon
    cargo-nextest
    cargo-llvm-cov
    cargo-deny
    cargo-shear
    cargo-msrv
    cargo-mutants
    cargo-hack
    cargo-insta
    typos
    taplo

    # Nix tooling.
    nixfmt
    statix
    deadnix

    # Runtime tools the tests shim or need.
    bash
    python3
    jq
    yq-go
    direnv
  ]);
}
