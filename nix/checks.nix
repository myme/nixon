{
  pkgs,
  craneLib,
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

  # --no-deps as ENGINEERING §3 specifies. Documenting dependencies as well
  # raced on the shared target/doc tree and failed intermittently.
  doc = craneLib.cargoDoc (
    commonArgs
    // {
      inherit cargoArtifacts;
      cargoDocExtraArgs = "--no-deps";
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

  # crane has no cargo-shear helper, but it still needs the vendored registry:
  # a sandboxed build has no network to reach crates.io with.
  shear = craneLib.mkCargoDerivation (
    commonArgs
    // {
      inherit cargoArtifacts;
      pnameSuffix = "-shear";
      buildPhaseCargoCommand = "cargo shear";
      nativeBuildInputs = (commonArgs.nativeBuildInputs or [ ]) ++ [ pkgs.cargo-shear ];
    }
  );

  # The man pages exist, render without troff warnings, and nothing but
  # `nixon` was installed alongside them.
  man =
    pkgs.runCommand "nixon-check-man"
      {
        nativeBuildInputs = [
          pkgs.man-db
          pkgs.groff
        ];
      }
      ''
        installed="$(ls ${nixon}/bin)"
        if [ "$installed" != "nixon" ]; then
          echo "expected only nixon in bin, found: $installed" >&2
          exit 1
        fi

        # nixpkgs' fixupPhase gzips installed man pages.
        for page in \
          ${nixon}/share/man/man1/nixon.1.gz \
          ${nixon}/share/man/man5/nixon.md.5.gz \
          ${nixon}/share/man/man7/nixon-picker.7.gz \
          ${nixon}/share/man/man7/nixon-shell.7.gz
        do
          MANROFFSEQ= man --warnings -l "$page" > /dev/null 2> warnings.txt
          if [ -s warnings.txt ]; then
            echo "$page renders with warnings:" >&2
            cat warnings.txt >&2
            exit 1
          fi
        done

        touch $out
      '';

  typos = runCheck "typos" [ pkgs.typos ] "typos";
  statix = runCheck "statix" [ pkgs.statix ] "statix check .";
  deadnix = runCheck "deadnix" [ pkgs.deadnix ] "deadnix --fail .";
  nixfmt = runCheck "nixfmt" [ pkgs.nixfmt ] "nixfmt --check .";

  # Reported, not gated (ENGINEERING §1.1).
  coverage = craneLib.cargoLlvmCov (
    commonArgs
    // {
      inherit cargoArtifacts;
      cargoLlvmCovExtraArgs = "--html --output-dir $out";
    }
  );
}
