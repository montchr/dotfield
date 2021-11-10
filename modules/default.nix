{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
    ./settings.nix

    ./php
  ];

  my.modules = {
    gui.enable = true;
  };
}
