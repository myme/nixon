{ pkgs ? import ./nixpkgs.nix {}
, haskellPackages ? pkgs.haskellPackages,
}:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  src = gitignore ./.;

in (haskellPackages.callCabal2nix "nixon" ./. {}).overrideAttrs (attrs: attrs // {
 executableSystemDepends = with pkgs; [
   fzf
   rofi
 ];
  postInstall = ''
    # Install widgets into share/nixon
    mkdir -p $out/share/nixon
    install ${src}/extra/nixon-widget.bash $out/share/nixon
    install ${src}/extra/nixon-widget.zsh $out/share/nixon

    # Bash completions
    mkdir -p $out/share/bash-completion/completions
    $out/bin/nixon --bash-completion-script $out/bin/nixon > $out/share/bash-completion/completions/nixon.bash

    # Install zsh widget + completions into share/zsh/site-functions
    mkdir -p $out/share/zsh/site-functions
    install ${src}/extra/nixon-widget.zsh $out/share/zsh/site-functions/_nixon_widget
    $out/bin/nixon --zsh-completion-script $out/bin/nixon > $out/share/zsh/site-functions/_nixon_completion
  '';
})
