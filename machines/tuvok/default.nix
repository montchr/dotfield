{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix
    ./hardware-configuration.nix
  ];

  time.timeZone = "America/New_York";

  users.mutableUsers = false;

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  # Not yet: <https://asahilinux.org/2024/01/fedora-asahi-new/#can-we-run-steam>
  # programs.steam.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
