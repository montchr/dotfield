{ config, pkgs, ... }:

{
  imports = [
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
    ./php
  ];

  my.modules = {
    espanso.enable = true;
    firefox.enable = true;
    gui.enable = true;
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
