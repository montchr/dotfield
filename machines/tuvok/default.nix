{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix

    ./webdev.nix
    ./keyboard.nix

    ./hardware-configuration.nix
  ];

  time.timeZone = "America/New_York";

  users.mutableUsers = false;

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  # Currently required for Asahi monitor support via USB-C.  Asahi does not yet
  # support DP-Alt display output.  DP-Alt output is required for true HDMI or
  # DP output via one of this machine's two USB-C ports and zero HDMI/DP ports.
  services.xserver.videoDrivers = [ "displaylink" ];

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
