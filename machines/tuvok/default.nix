{ ... }:
{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix

    ./experimental-mesa.nix
    ./keyboard.nix
    ./work/default.nix

    ./hardware-configuration.nix
  ];

  # XXX: Will be fixed in kernel release 6.9.12-2
  # <https://github.com/tpwrules/nixos-apple-silicon/issues/225>
  # <https://social.treehouse.systems/@AsahiLinux/112909897657710314>
  # <https://bugzilla.redhat.com/show_bug.cgi?id=2302577>
  boot.kernelParams = [ "brcmfmac.feature_disable=0x82000" ];

  lix.enable = true;

  time.timeZone = "America/New_York";

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  services.tailscale.enable = true;

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
