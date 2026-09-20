{ pkgs, craneLib }:

let
  # crane's cargo filter keeps only what cargo needs to build. Everything else
  # the build or the tests read has to be named here, or the checks see a
  # different tree than `cargo test` does: .snap files, or insta finds no
  # stored snapshot and every snapshot test is "new"; docs/, which pandoc
  # renders into man pages and a test reads to prove the documented --help has
  # not drifted.
  keep = path: builtins.match ".*(\\.snap|/docs/[^/]*\\.md)$" path != null;

  src = pkgs.lib.cleanSourceWith {
    src = ./..;
    name = "source";
    filter = path: type: (keep path) || (craneLib.filterCargoSources path type);
  };

  commonArgs = {
    inherit src;
    strictDeps = true;
    nativeBuildInputs = [ pkgs.installShellFiles ];
  };

  # Built once and reused by every check, so a code change never rebuilds
  # dependencies.
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;

  nixon = craneLib.buildPackage (
    commonArgs
    // {
      inherit cargoArtifacts;

      # The widgets are v2's: no -b/-T, which no longer parse. Completion is
      # clap_complete's CompleteEnv, so the loaders are `eval`'d snippets
      # rather than generated files.
      postInstall = ''
        install -Dm444 -t $out/share/nixon \
          ${../extra}/nixon-widget.bash \
          ${../extra}/nixon-widget.zsh \
          ${../extra}/nixon-widget.fish

        mkdir -p $out/share/bash-completion/completions
        echo 'source <(COMPLETE=bash nixon)' \
          > $out/share/bash-completion/completions/nixon

        mkdir -p $out/share/zsh/site-functions
        echo 'source <(COMPLETE=zsh nixon)' \
          > $out/share/zsh/site-functions/_nixon

        mkdir -p $out/share/fish/vendor_completions.d
        echo 'COMPLETE=fish nixon | source' \
          > $out/share/fish/vendor_completions.d/nixon.fish

      '';

      meta = {
        description = "Project environment and command launcher";
        homepage = "https://github.com/myme/nixon";
        license = pkgs.lib.licenses.mit;
        mainProgram = "nixon";
      };
    }
  );
in
{
  inherit nixon commonArgs cargoArtifacts;
}
