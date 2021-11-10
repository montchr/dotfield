{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
    ./settings.nix
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
    tealdeer.enable = true;

    editors = {
      vim.enable = true;
    };
  };
}
