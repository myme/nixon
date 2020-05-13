let
  nixpkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-20.03";
    rev = "5adf2a6c11646898742b0c08f7e94101620ba707";
  });

  koi = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-19.03";
    rev = "c8db7a8a16ee9d54103cade6e766509e1d1c8d7b";
  });

in {
  inherit nixpkgs koi;
}
