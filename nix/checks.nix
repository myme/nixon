{
  pkgs,
  craneLib,
  toolchain,
  nixon,
  commonArgs,
  cargoArtifacts,
}:

let
  inherit (pkgs) lib;

  # Nix and repo-wide checks run over the whole tree, not the cargo source.
  repoSrc = lib.cleanSource ./..;

  runCheck =
    name: deps: script:
    pkgs.runCommand "nixon-check-${name}" { nativeBuildInputs = deps; } ''
      export HOME="$(mktemp -d)"
      cd ${repoSrc}
      ${script}
      touch $out
    '';
in
{
  build = nixon;

  clippy = craneLib.cargoClippy (
    commonArgs
    // {
      inherit cargoArtifacts;
      cargoClippyExtraArgs = "--all-targets -- -D warnings";
    }
  );

  fmt = craneLib.cargoFmt { inherit (commonArgs) src; };

  toml-fmt = craneLib.taploFmt {
    src = lib.sources.sourceFilesBySuffices ./.. [ ".toml" ];
  };

  doc = craneLib.cargoDoc (
    commonArgs
    // {
      inherit cargoArtifacts;
      env.RUSTDOCFLAGS = "-D warnings";
    }
  );

  test = craneLib.cargoNextest (
    commonArgs
    // {
      inherit cargoArtifacts;
      partitions = 1;
      partitionType = "count";
    }
  );

  deny = craneLib.cargoDeny { inherit (commonArgs) src; };

  # crane has no helper for these.
  shear = runCheck "shear" [
    toolchain
    pkgs.cargo-shear
  ] "cargo shear";
  typos = runCheck "typos" [ pkgs.typos ] "typos";
  statix = runCheck "statix" [ pkgs.statix ] "statix check .";
  deadnix = runCheck "deadnix" [ pkgs.deadnix ] "deadnix --fail .";
  nixfmt = runCheck "nixfmt" [ pkgs.nixfmt-rfc-style ] "nixfmt --check .";

  # Reported, not gated (ENGINEERING §1.1).
  coverage = craneLib.cargoLlvmCov (
    commonArgs
    // {
      inherit cargoArtifacts;
      cargoLlvmCovExtraArgs = "--html --output-dir $out";
    }
  );
}
