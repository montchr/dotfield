{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
    ./settings.nix

    ./editors/vim
    ./firefox
    ./php
  ];

  my.modules = {
    firefox.enable = true;
    gui.enable = true;

    editors = {
      vim.enable = true;
    };
  };
}
