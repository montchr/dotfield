{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
    ./settings.nix
    ./tealdeer.nix

    ./editors/vim
    ./firefox
    ./php
  ];

  my.modules = {
    firefox.enable = true;
    gui.enable = true;
    tealdeer.enable = true;

    editors = {
      vim.enable = true;
    };
  };
}
