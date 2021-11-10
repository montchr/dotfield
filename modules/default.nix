{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
    ./settings.nix
  ];

  my.modules = {
    gui.enable = true;
  };
}
