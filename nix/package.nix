{ pkgs, craneLib }:

let
  src = craneLib.cleanCargoSource ./..;

  commonArgs = {
    inherit src;
    strictDeps = true;
    nativeBuildInputs = [ pkgs.installShellFiles ];
  };

  # Built once and reused by every check, so a code change never rebuilds
  # dependencies (ENGINEERING §1.1).
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;

  nixon = craneLib.buildPackage (
    commonArgs
    // {
      inherit cargoArtifacts;

      # TODO: install the rewritten shell widgets and completion loaders here
      # once they exist (ENGINEERING §7.2). The v1 widgets are incompatible.
      postInstall = ''
        mkdir -p $out/share/nixon
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
