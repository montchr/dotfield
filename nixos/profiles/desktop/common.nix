{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.dotfield.sys) hasWayland;

  firefoxPackage =
    if hasWayland
    then pkgs.firefox-wayland
    else pkgs.firefox;
in {
  services.xserver.enable = true;
  services.xserver.layout = "us";
  # TODO: tap-dance: esc
  services.xserver.xkbOptions = "caps:ctrl_modifier";
  dotfield.guardian.user.extraGroups = ["video"];

  xdg.portal.enable = true;


  # FIXME: still necessary? this isn't a great idea
  # programs.gnupg.agent.enableBrowserSocket = true;

  security.sudo.wheelNeedsPassword = false;

  # Hide cursor upon keystroke.
  services.unclutter = {
    enable = true;
    keystroke = true;
  };

  environment.variables = {
    MOZ_ENABLE_WAYLAND = lib.optionalString hasWayland "1";
    # Enable macOS-like smooth scrolling instead of the weird scroll-wheel emulation.
    # https://wiki.archlinux.org/title/Firefox/Tweaks#Pixel-perfect_trackpad_scrolling
    MOZ_USE_XINPUT2 = "1";
  };

  environment.systemPackages =
    (with pkgs; [
      firefoxPackage
      signal-desktop
    ])
    ++ (lib.optionals hasWayland (with pkgs; [
      wl-clipboard

      # Grab images from a Wayland compositor
      # https://sr.ht/~emersion/grim/
      grim

      # Select a region in a Wayland compositor and print it to the standard output.
      # A complement to grim
      # https://github.com/emersion/slurp
      slurp
    ]));
}
