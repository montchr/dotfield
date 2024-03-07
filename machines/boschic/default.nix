{
  config,
  lib,
  ops,
  ...
}:
let
  inherit (ops) hosts networks;
  inherit (config.networking) hostName;
in
{
  imports = [
    ./hardware-configuration.nix
    ./profiles/sops.nix
    ./users
  ];

  time.timeZone = "America/New_York";

  # FIXME: disable. likely interferes with rEFInd.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 15;
  boot.initrd.supportedFilesystems = [ "btrfs" ];
  boot.supportedFilesystems = [ "btrfs" ];

  virtualisation.vmVariant = {
    virtualisation.graphics = false;
  };

  system.stateVersion = "21.11";

  ### === networking ===========================================================

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
}
