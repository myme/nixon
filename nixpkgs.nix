let
  nixpkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-19.09";
    rev = "2de9367299f325c2b2021a44c2f63c810f8ad023";
  });

  koi = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-19.03";
    rev = "c8db7a8a16ee9d54103cade6e766509e1d1c8d7b";
  });

in {
  inherit nixpkgs koi;
}
