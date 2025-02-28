{ flake, pkgs, ... }:
let
  wlPkgs = flake.perSystem.inputs'.nixpkgs-wayland.packages;
in
{
  imports = [
    ../common.nix
  ];

  security.pam.services.swaylock = { };
  security.pam.services.waylock = { };

  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  xdg.portal.wlr.enable = true;

  # TODO: provide a default launcher
  environment.systemPackages = with pkgs; [
    wlPkgs.gtk-layer-shell

    # essentials
    brightnessctl
    wlPkgs.grim
    wlPkgs.slurp
    wev # input monitoring
    wl-clipboard # clipboard

    # swappables
    nemo # file manager

    # TODO: evaluate
    shikane # aims to be improvement over kanshi
    wluma # sensor-adaptive brightness adjustment daemon

  ];
}
