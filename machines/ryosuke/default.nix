{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./game.nix
    ./media.nix
    ./users/cdom.nix
    ./users/median.nix
  ];

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  sops.defaultSopsFile = ./secrets/secrets.yaml;

  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.supportedFilesystems = [ "btrfs" ];
  boot.supportedFilesystems = [ "btrfs" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  virtualisation.vmVariant = {
    virtualisation.graphics = true;
  };

  system.stateVersion = "22.05";

  # Set your time zone.
  time.timeZone = "America/New_York";

  networking.firewall.enable = true;

  ##: wake on lan
  # TODO: no idea if this works
  # networking.interfaces."eth0".wakeOnLan.enable = true;
  # # https://wiki.archlinux.org/title/Wake-on-LAN#Enable_WoL_in_TLP
  # services.tlp.settings.WOL_DISABLE = "N";
  # environment.systemPackages = with pkgs; [
  #   # Manually enable WOL:
  #   #   $ sudo ethtool -s eth0 wol g
  #   # Check WOL status:
  #   #   $ sudo ethtool eth0 | grep Wake-on
  #   ethtool
  # ];
}
