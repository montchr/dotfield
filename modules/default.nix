{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
    ./settings.nix

    ./firefox
    ./php
  ];

  my.modules = {
    firefox.enable = true;
    gui.enable = true;
  };
}
