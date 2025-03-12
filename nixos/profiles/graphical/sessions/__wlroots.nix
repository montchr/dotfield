{ flake, pkgs, ... }:
let
  wlPkgs = flake.perSystem.inputs'.nixpkgs-wayland.packages;
in
{
  imports = [
    ../nixpkgs-wayland-overlay.nix
    ../common.nix
  ];

  # Required for lockers to perform authentication.
  security.pam.services.swaylock = { };
  security.pam.services.waylock = { };

  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  xdg.portal.wlr.enable = true;

  # TODO: provide a default launcher
  environment.systemPackages = with pkgs; [
    gtk-layer-shell

    # essentials
    brightnessctl
    grim
    satty
    slurp
    swaybg
    swaylock
    swaylock-effects
    wev
    wf-recorder
    wl-clipboard

    # swappables
    kanshi
    nemo # file manager

    # TODO: evaluate
    nwg-displays # output management gui
    shikane # aims to be improvement over kanshi
    wluma # sensor-adaptive brightness adjustment daemon

  ];
}
