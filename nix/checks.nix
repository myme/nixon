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

  # --no-deps: documenting dependencies as well raced on the shared
  # target/doc tree and failed intermittently. -j1 for the same reason a
  # level down — rustdoc's own workers raced each other on macOS, failing
  # to write a file into a directory another had not finished creating:
  #
  #   error: ".../target/doc/nixon/config/enum.LogLevel.html": No such file
  #   error: couldn't generate documentation: I/O error
  #
  # Serial is slower and always works. A target dir of its own is the
  # fallback if this ever comes back.
  doc = craneLib.cargoDoc (
    commonArgs
    // {
      inherit cargoArtifacts;
      cargoDocExtraArgs = "--no-deps -j1";
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
          pkgs.gzip
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
          # whatis and `man -k` read the NAME section; without one the page
          # is invisible to them.
          if ! zcat "$page" | grep -q '^\.SH NAME'; then
            echo "$page has no NAME section" >&2
            exit 1
          fi
        done

        touch $out
      '';

  # The installed `_nixon` is something compinit can register, and it
  # completes on the first Tab rather than only arming itself for the next.
  completion =
    pkgs.runCommand "nixon-check-completion"
      {
        nativeBuildInputs = [
          pkgs.zsh
          pkgs.util-linux
        ];
      }
      ''
        export HOME=$PWD
        fpath_dir=${nixon}/share/zsh/site-functions
        test -f "$fpath_dir/_nixon" || { echo "no _nixon installed" >&2; exit 1; }

        export ZDOTDIR=$PWD
        cat > .zshrc <<EOF
        fpath=($fpath_dir \$fpath)
        autoload -Uz compinit
        compinit -D -i
        print -r -- "comps=\''${_comps[nixon]-NONE}"
        PS1='ready> '
        EOF

        # A real Tab at a real prompt: `_nixon` is autoloaded by compinit,
        # so nothing here sources it. `script` supplies the tty completion
        # needs.
        PATH=${nixon}/bin:$PATH
        printf 'nixon \t\n\nexit\n' \
          | script -qec "zsh -i" /dev/null > out.txt 2>&1 || true
        cat -v out.txt

        grep -q 'comps=_nixon' out.txt || {
          echo "compinit did not register _nixon for nixon" >&2
          exit 1
        }
        grep -q 'history' out.txt || {
          echo "Tab offered no subcommands" >&2
          exit 1
        }

        touch $out
      '';

  # Only the bash widget: shellcheck has no zsh or fish support, and
  # checking those as bash reports their own syntax as errors.
  shellcheck = runCheck "shellcheck" [
    pkgs.shellcheck
  ] "shellcheck --shell=bash extra/*.bash";

  typos = runCheck "typos" [ pkgs.typos ] "typos";
  statix = runCheck "statix" [ pkgs.statix ] "statix check .";
  deadnix = runCheck "deadnix" [ pkgs.deadnix ] "deadnix --fail .";
  nixfmt = runCheck "nixfmt" [ pkgs.nixfmt ] "nixfmt --check .";

  # Reported, not gated.
  coverage = craneLib.cargoLlvmCov (
    commonArgs
    // {
      inherit cargoArtifacts;
      cargoLlvmCovExtraArgs = "--html --output-dir $out";
    }
  );
}
