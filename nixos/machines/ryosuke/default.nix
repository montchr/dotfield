{
  config,
  lib,
  pkgs,
  peers,
  ...
}: let
  inherit (config.networking) hostName;
in {
  imports = [
    ./hardware-configuration.nix
    ./profiles/sops.nix
    ./users
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.supportedFilesystems = ["btrfs"];
  boot.supportedFilesystems = ["btrfs"];

  virtualisation.vmVariant = {
    virtualisation.graphics = true;
  };

  system.stateVersion = "22.05";

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Enable sound with pipewire.
  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  ### === networking ===========================================================

  networking.firewall.enable = true;
  networking.useDHCP = true;
  networking.usePredictableInterfaceNames = false;

  ##: wake on lan

  networking.interfaces."eth0".wakeOnLan.enable = true;
  # https://wiki.archlinux.org/title/Wake-on-LAN#Enable_WoL_in_TLP
  services.tlp.settings.WOL_DISABLE = "N";
  environment.systemPackages = with pkgs; [
    # Manually enable WOL:
    #   $ sudo ethtool -s eth0 wol g
    # Check WOL status:
    #   $ sudo ethtool eth0 | grep Wake-on
    ethtool
  ];
}
