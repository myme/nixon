let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = import (fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "851d5bdfb04aa1f1f8e2a89323cdb9ba03daba99";
    sha256 = "0srrspy7bpdywywy8w95zxkh8j7xsvq58msgsqymm5fphsfsyad6";
  });
in nixpkgs
