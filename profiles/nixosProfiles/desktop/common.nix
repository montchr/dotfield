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
  imports = [./nixpkgs-wayland.nix];

  services.xserver.enable = true;
  services.xserver.layout = "us";
  dotfield.guardian.user.extraGroups = ["audio" "video"];

  xdg.portal.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = false; # required for pipewire
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
  };

  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluez;

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  security.sudo.wheelNeedsPassword = false;

  # Hide cursor upon keystroke.
  services.unclutter = {
    enable = true;
    keystroke = true;
  };

  # Prevent stupid boot delays waiting for internet.
  # FIXME: this doesn't really seem to help much. dhcp still delays boot.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;

  environment.systemPackages =
    [
      # We always need a browser on desktop.
      pkgs.firefox

      # Provide a minimal and sensible default terminal emulator as a fallback
      # in case the desktop environment doesn't bundle its own application.
      pkgs.foot
    ]
    ++ (l.optional (!isAarch64) pkgs.signal-desktop) # <- broken
    ++ (l.optional hasWayland pkgs.wl-clipboard);
}
