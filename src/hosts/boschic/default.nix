{ config, self, ... }:
let
  inherit (config.meta) hosts networks;

  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.boschic = {
    system = "x86_64-linux";
    aspects = with config.aspects; [ graphical ];
    configuration = {
      imports = [
        (nixos + "/mixins/gnome.nix")
        (nixos + "/mixins/jobwork.nix")
        (nixos + "/mixins/workstation.nix")

        # FIXME: clarify that this means an amd cpu, NOT gpu
        (nixos + "/profiles/hardware/amd.nix")

        (nixos + "/profiles/hardware/focusrite-scarlett-18i20-mk1.nix")

        # TODO: rename to note that this is gpu, making it mutually exclusive
        #       with an AMD GPU (same goes for intel/amd cpu but i don't bother
        #       with intel cpus)
        (nixos + "/profiles/hardware/nvidia/stable-release.nix")
        (nixos + "/profiles/hardware/razer.nix")
      ];

      dotfield.features.hasNvidia = true;

      time.timeZone = "America/New_York";

      # FIXME: disable. likely interferes with rEFInd.
      boot.loader.efi.canTouchEfiVariables = true;

      boot.loader.timeout = 15;
      boot.initrd.supportedFilesystems = [ "btrfs" ];
      boot.supportedFilesystems = [ "btrfs" ];

      services.flatpak.enable = true;

      virtualisation.vmVariant = {
        virtualisation.graphics = false;
      };

      dotfield.guardian.enable = true;
      dotfield.guardian.username = "seadoom";
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

      home-manager.sharedModules = [
        (self.outPath + "/home/mixins/nvidia.nix")
      ];

      system.stateVersion = "21.11";
    };
  };
}
