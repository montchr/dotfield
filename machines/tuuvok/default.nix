{ lib, flake, ... }:
{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix

    ./backups.nix
    ./keyboard.nix
    ./work/default.nix

    ./hardware-configuration.nix
  ];

  fonts.packages = [ flake.perSystem.packages.berkeley-mono ];

  # NOTE: The firmware "asahi-tuuvok-firmware" repository results in
  # broken wifi.  Reverting to the "asahi-tuvok-firmware" repository works.
  hardware.asahi.peripheralFirmwareDirectory =
    flake.perSystem.inputs'.asahi-tuvok-firmware.packages.default;

  time.timeZone = "America/New_York";

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "cdom";

  # Not allowed because I don't want to make the building's network
  # switch mad again.
  # TODO: Should be disabled by default?
  services.avahi.enable = lib.mkForce false;

  services.tailscale.enable = true;

  system.stateVersion = "23.11"; # Did you read the comment?
}
{ }
