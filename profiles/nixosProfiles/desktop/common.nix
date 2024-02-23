{
  config,
  flake,
  pkgs,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./firefox.nix
    ./nixpkgs-wayland.nix
  ];

  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";

  # FIXME: guardian should set these automatically, be very careful
  dotfield.guardian.user.extraGroups = ["audio" "video"];

  xdg.portal.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  # Always support GTK applications.
  # programs.dconf.enable = true;

  security.sudo.wheelNeedsPassword = false;

  # Hide cursor upon keystroke.
  # FIXME: "could not open display"
  # services.unclutter = {
  #   enable = true;
  #   keystroke = true;
  # };

  # Prevent stupid boot delays waiting for internet.
  # FIXME: this doesn't really seem to help much. dhcp still delays boot.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;

  environment.systemPackages =
    [
      # Provide a minimal and sensible default terminal emulator as a fallback
      # in case the desktop environment doesn't bundle its own application or
      # the user config doesn't specify one.
      pkgs.foot

      pkgs.wl-clipboard
    ]
    ++ (l.optional (!isAarch64) pkgs.signal-desktop); # <- broken
}
