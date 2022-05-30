{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome.gnome-tweaks

    ##: gnome extensions -------------------------------------------------------

    # A glimpse into your computer's temperature, voltage, fan speed, memory
    # usage and CPU load.
    # https://github.com/corecoding/Vitals
    gnomeExtensions.vitals

    ##: nixos<>gnome helpers ---------------------------------------------------

    # https://github.com/gvolpe/dconf2nix
    dconf2nix
  ];
}
