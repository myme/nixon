{ pkgs, craneLib }:

let
  # crane's cargo filter keeps only what cargo needs to build. Everything else
  # the build or the tests read has to be named here, or the checks see a
  # different tree than `cargo test` does: .snap files, or insta finds no
  # stored snapshot and every snapshot test is "new"; docs/, which pandoc
  # renders into man pages and a test reads to prove the documented --help has
  # not drifted; and extra/, which a test sources into a real bash to drive
  # the shell widget.
  keep = path: builtins.match ".*(\\.snap|/docs/.*\\.md|/extra/nixon-widget\\..*)$" path != null;

  src = pkgs.lib.cleanSourceWith {
    src = ./..;
    name = "source";
    filter = path: type: (keep path) || (craneLib.filterCargoSources path type);
  };

  commonArgs = {
    inherit src;
    strictDeps = true;
    nativeBuildInputs = [
      pkgs.installShellFiles
      # setsid, for the test that runs nixon with no controlling terminal.
      pkgs.util-linux
      # The worktree fixtures are checked against real git once.
      pkgs.git
      # A bash with readline, and the terminfo it needs. stdenv's bash is
      # built without readline, so the widget test's key binding arrives as
      # literal text rather than triggering anything.
      pkgs.bashInteractive
      pkgs.ncurses
    ];
  };

  # Built once and reused by every check, so a code change never rebuilds
  # dependencies.
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;

  nixon = craneLib.buildPackage (
    commonArgs
    // {
      inherit cargoArtifacts;

      nativeBuildInputs = commonArgs.nativeBuildInputs ++ [ pkgs.pandoc ];

      # The widgets are v2's: no -b/-T, which no longer parse. Completion is
      # clap_complete's CompleteEnv, so the loaders are `eval`'d snippets
      # rather than generated files.
      #
      # Man pages have two sources and one origin: nixon(1) comes from the
      # clap command tree via the hidden `internal mangen` subcommand, the
      # rest from the same docs/ pages the repository serves. Nothing
      # generated is committed.
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

        mkdir -p $out/share/man/man1 $out/share/man/man5 $out/share/man/man7
        $out/bin/nixon internal mangen > $out/share/man/man1/nixon.1

        pandoc -s -f gfm -t man \
          --metadata title=nixon.md --metadata section=5 \
          docs/man/nixon.md.5.md \
          docs/configuration.md docs/commands.md docs/placeholders.md \
          -o $out/share/man/man5/nixon.md.5

        pandoc -s -f gfm -t man \
          --metadata title=nixon-picker --metadata section=7 \
          docs/man/nixon-picker.7.md docs/picker.md \
          -o $out/share/man/man7/nixon-picker.7

        pandoc -s -f gfm -t man \
          --metadata title=nixon-shell --metadata section=7 \
          docs/man/nixon-shell.7.md docs/shell-integration.md \
          -o $out/share/man/man7/nixon-shell.7
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
