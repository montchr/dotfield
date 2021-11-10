{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
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
    ssh.enable = true;
    tealdeer.enable = true;

    editors = {
      vim.enable = true;
    };
  };
}
