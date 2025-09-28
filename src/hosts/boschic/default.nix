{ config, self, ... }:
let
  inherit (config.meta) hosts networks;
in
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

      ### === networking ===========================================================

      services.tailscale.enable = true;

      # FIXME: no connection on boot -- i need to disable internet and re-enable
      # every time despite indication of a wired connection in GNOME status bar
      networking =
        let
          host = hosts.boschic;
          net = networks.${host.network};
          interface = "eth0";
        in
        {
          useDHCP = false;
          usePredictableInterfaceNames = false;
          # interfaces.wlp6s0.useDHCP = true;

          firewall = {
            enable = true;
            # allowedTCPPorts = [80 443];
          };

          defaultGateway = {
            inherit interface;
            inherit (net.ipv4) address;
          };

          interfaces.${interface} = {
            ipv4.addresses = [
              {
                inherit (host.ipv4) address;
                inherit (net.ipv4) prefixLength;
              }
            ];
          };
        };

      programs.steam.enable = true;

      sops.defaultSopsFile = ./secrets/secrets.yaml;

      system.stateVersion = "21.11";
    };
  };
}
