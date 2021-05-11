{ config, pkgs, ... }:

{
  # Roughly in order of precedence.
  imports = [
    ./settings.nix
    ./zsh.nix
    ./bash.nix
    ./direnv.nix
    ./gpg.nix
    ./ssh.nix
    ./git.nix
    ./kitty.nix
    ./tealdeer.nix
    ./bat.nix
    ./ripgrep.nix
    ./node.nix
    ./gui.nix
    ./python.nix
  ];

  my.modules = {
    bat.enable = true;
    bash.enable = true;
    direnv.enable = true;
    git.enable = true;
    gui.enable = true;
    kitty.enable = true;
    node.enable = true;
    python.enable = true;
    ripgrep.enable = true;
    ssh.enable = true;
    tealdeer.enable = true; # rust implementation of tldr
    zsh.enable = true;
  };

}
