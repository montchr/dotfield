{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.xwayland.enable = true;

  xdg.portal.extraPortals = with pkgs; [xdg-desktop-portal-wlr];

  environment.systemPackages = with pkgs; [
    clipman

    # Grab images from a Wayland compositor
    # https://sr.ht/~emersion/grim/
    grim

    # Select a region in a Wayland compositor and print it to the standard output.
    # A complement to grim
    # https://github.com/emersion/slurp
    slurp

    wl-clipboard
  ];
}
