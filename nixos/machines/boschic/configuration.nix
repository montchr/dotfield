{
  config,
  lib,
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

  # FIXME: does this interfere with rEFInd? if not this, then i blame Windows.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 15;
  boot.initrd.supportedFilesystems = ["btrfs"];
  boot.supportedFilesystems = ["btrfs"];

  virtualisation.vmVariant = {
    virtualisation.graphics = false;
  };

  system.stateVersion = "21.11";

  ### === networking ===========================================================

  networking = lib.mkIf (!config.nixos-vm.enable) (
    let
      host = peers.hosts.${hostName};
      net = peers.networks.${host.network};
      interface = "eth0";
    in {
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
    }
  );

  programs.steam.enable = true;
}
