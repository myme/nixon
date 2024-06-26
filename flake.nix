{
  description = "Nixon nix flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in {
      overlay = (final: prev: {
        nixon = pkgs.haskell.lib.compose.justStaticExecutables
          final.haskellPackages.nixon;
        haskellPackages = prev.haskellPackages // {
          nixon = import ./default.nix {
            pkgs = final;
            inherit (final) haskellPackages;
          };
        };
      });

      defaultPackage.${system} = pkgs.nixon;

      devShell.${system} = import ./shell.nix {
        inherit pkgs;
        inherit (pkgs) haskellPackages;
      };
    };
}
