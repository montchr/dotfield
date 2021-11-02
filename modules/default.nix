{ config, pkgs, ... }:

{
  imports = [
    ./bat.nix
    ./direnv.nix
    ./git.nix
    ./gui.nix
    ./node.nix
    ./python.nix
    ./ripgrep.nix
    ./settings.nix
    ./ssh.nix
    ./tealdeer.nix

    ./editors/vim
    ./espanso
    ./firefox
    ./kitty
    ./php
  ];

  my.modules = {
    bat.enable = true;
    direnv.enable = true;
    espanso.enable = true;
    firefox.enable = true;
    git.enable = true;
    gui.enable = true;
    kitty.enable = true;
    node.enable = true;
    python.enable = true;
    ripgrep.enable = true;
    ssh.enable = true;
    tealdeer.enable = true;

    editors = {
      vim.enable = true;
    };
  };

}
