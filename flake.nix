{
  description = "Nixon nix flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

  outputs = { self, nixpkgs }:
    let system = "x86_64-linux";
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          overlays = [self.overlay];
        };
    in {
      overlay = (final: prev: {
        nixon = import ./default.nix {
          pkgs = final;
          inherit (final) haskellPackages;
        };
      });

      defaultPackage.${system} = pkgs.nixon;

      devShell.${system} = import ./shell.nix {
        inherit pkgs;
        inherit (pkgs) haskellPackages;
      };
  };
}
