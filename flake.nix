{
  description = "Nixon nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    {
      overlay = (
        final: prev: {
          nixon = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.nixon;
          haskellPackages = prev.haskellPackages // {
            nixon = import ./default.nix {
              pkgs = final;
              inherit (final) haskellPackages;
            };
          };
        }
      );
    }
    // (utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in
      {
        defaultPackage = pkgs.nixon;
        devShell = import ./shell.nix {
          inherit pkgs;
          inherit (pkgs) haskellPackages;
        };
      }
    ));
}
