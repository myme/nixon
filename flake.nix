{
  description = "Nixon nix flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux = (import nixpkgs { system = "x86_64-linux"; }).callPackage ./default.nix {};

  };
}
