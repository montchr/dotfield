{ config, self, ... }:
{
  hosts.nixos.boschic = {
    system = "x86_64-linux";
    aspects = with config.aspects; [
      workstation
      desktop-sessions__gnome
      hardware__amd__cpu
      hardware__nvidia
      development__kleinweb
    ];
    configuration = {
      time.timeZone = "America/New_York";

      # FIXME: disable. likely interferes with rEFInd.
      boot.loader.efi.canTouchEfiVariables = true;

      boot.loader.timeout = 15;
      boot.initrd.supportedFilesystems = [ "btrfs" ];
      boot.supportedFilesystems = [ "btrfs" ];

      virtualisation.vmVariant = {
        virtualisation.graphics = false;
      };

      users.mutableUsers = false;

      programs.steam.enable = true;

      sops.defaultSopsFile = ./secrets/secrets.yaml;

      system.stateVersion = "21.11";
    };
  };
}
