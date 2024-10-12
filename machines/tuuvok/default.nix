{ flake, ... }:
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

  time.timeZone = "America/New_York";

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  services.tailscale.enable = true;

  # TODO: relocate to a relevant profile -- why was this necessary?
  # programs.appimage = {
  #   enable = true;
  #   binfmt = true;
  # };

  system.stateVersion = "23.11"; # Did you read the comment?
}
