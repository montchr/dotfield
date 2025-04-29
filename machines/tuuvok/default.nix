{ flake, ... }:
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

  services.tailscale.enable = true;

  system.stateVersion = "23.11"; # Did you read the comment?
}
