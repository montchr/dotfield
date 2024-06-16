{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix

    ./keyboard.nix
    ./work/default.nix

    ./hardware-configuration.nix
  ];

  time.timeZone = "America/New_York";

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  # Currently required for Asahi monitor support via USB-C.  Asahi does not yet
  # support DP-Alt display output.  DP-Alt output is required for true HDMI or
  # DP output via one of this machine's two USB-C ports and zero HDMI/DP ports.
  services.xserver.videoDrivers = [ "displaylink" ];

  system.stateVersion = "23.11"; # Did you read the comment?
}
