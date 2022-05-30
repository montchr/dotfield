{ config, lib, pkgs, ... }:

{
  imports = [./dconf.settings.nix];

  home.packages = with pkgs; [
    ##: nixos<>gnome helpers ---------------------------------------------------

    # https://github.com/gvolpe/dconf2nix
    dconf2nix
  ];
}
