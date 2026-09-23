{
  description = "Nixon — project environment and command launcher";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      crane,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        # One toolchain for nix, CI and a bare cargo on a rustup machine.
        toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        craneLib = (crane.mkLib pkgs).overrideToolchain toolchain;

        package = import ./nix/package.nix { inherit pkgs craneLib; };
      in
      {
        packages = {
          default = package.nixon;
          inherit (package) nixon;
        };

        checks = import ./nix/checks.nix {
          inherit pkgs craneLib;
          inherit (package) nixon commonArgs cargoArtifacts;
        };

        devShells.default = import ./nix/shell.nix {
          inherit pkgs craneLib toolchain;
          inherit (package) guiRuntimeLibraries;
        };

        formatter = pkgs.nixfmt;
      }
    )
    // {
      overlays.default = final: _prev: {
        nixon = self.packages.${final.system}.nixon;
      };
    };
}
