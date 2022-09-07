{
  config,
  lib,
  pkgs,
  collective,
  ...
}: let
  inherit (collective) peers;
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
    virtualisation.graphics = false;
  };

  system.stateVersion = "22.05";

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  ### === networking ===========================================================

  networking = lib.mkIf (!config.nixos-vm.enable) (
    let
      #host = peers.hosts.${hostName};
      #net = peers.networks.${host.network};
      #interface = "eth0";
    in {
      #networkmanager.enable = true;
      #wireless.enable = true; # Enables wireless support via wpa_supplicant.
      #useDHCP = true;
      #usePredictableInterfaceNames = false;

      firewall = {
        enable = true;
        # allowedTCPPorts = [80 443];
      };
    }
  );
}
