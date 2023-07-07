{
  config,
  flake,
  pkgs,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [./nixpkgs-wayland.nix];

  services.xserver.enable = true;
  services.xserver.layout = "us";
  # TODO: tap-dance: esc
  services.xserver.xkbOptions = "caps:ctrl_modifier";
  dotfield.guardian.user.extraGroups = ["audio" "video"];

  xdg.portal.enable = true;

  # TODO: make opt-in: default to disabled on asahi
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

  # TODO: make opt-in: default to disabled on asahi
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

  environment.variables = {
    # MOZ_ENABLE_WAYLAND = l.optionalString hasWayland "1";
    # Enable macOS-like smooth scrolling instead of the weird scroll-wheel emulation.
    # https://wiki.archlinux.org/title/Firefox/Tweaks#Pixel-perfect_trackpad_scrolling
    # MOZ_USE_XINPUT2 = "1";
  };

  environment.systemPackages =
    [
      pkgs.firefox
      # FIXME: broken on aarch64-linux
      # pkgs.signal-desktop
    ]
    ++ (l.optional hasWayland pkgs.wl-clipboard);
}
