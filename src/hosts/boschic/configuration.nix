flake@{ lib, ... }:
{
  dotfield.hosts.boschic =
    { config, ... }:
    let
      inherit (flake.config.dotfield.meta) hosts networks;
      inherit (config.networking) hostName;
    in
    {
      dotfield.features.hasNvidia = true;

      # FIXME: disable. likely interferes with rEFInd.
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.timeout = 15;
      boot.initrd.supportedFilesystems = [ "btrfs" ];
      boot.supportedFilesystems = [ "btrfs" ];

      services.flatpak.enable = true;

      virtualisation.vmVariant = {
        virtualisation.graphics = false;
      };

      system.stateVersion = "21.11";

      ### === networking ===========================================================

      services.tailscale.enable = true;

      # FIXME: no connection on boot -- i need to disable internet and re-enable
      # every time despite indication of a wired connection in GNOME status bar
      networking =
        let
          host = hosts.${hostName};
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

      home-manager.sharedModules = [
        ../../home/mixins/nvidia.nix
      ];
    };
}
