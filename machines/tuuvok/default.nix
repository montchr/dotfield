{ flake, pkgs, ... }:
{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix

    ./backups.nix

    ./experimental-mesa.nix

    ./keyboard.nix
    ./work/default.nix

    ./hardware-configuration.nix
  ];

  # NOTE: The firmware "asahi-tuuvok-firmware" repository results in
  # broken wifi.  Reverting to the "asahi-tuvok-firmware" repository works.
  hardware.asahi.peripheralFirmwareDirectory =
    flake.perSystem.inputs'.asahi-tuvok-firmware.packages.default;

  environment.systemPackages = [ pkgs.borgbackup ];

  lix.enable = true;

  time.timeZone = "America/New_York";

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  # services.tailscale.enable = true;

  # Currently required for Asahi monitor support via USB-C.  Asahi does not yet
  # support DP-Alt display output.  DP-Alt output is required for true HDMI or
  # DP output via one of this machine's two USB-C ports and zero HDMI/DP ports.
  # For details on update procedure, see <https://wiki.nixos.org/wiki/Displaylink>.
  services.xserver.videoDrivers = [
    "displaylink"
    "modesetting"
  ];

  programs.appimage = {
    enable = true;
    binfmt = true;
  };

  system.stateVersion = "23.11"; # Did you read the comment?
}
