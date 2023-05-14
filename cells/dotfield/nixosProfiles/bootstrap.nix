{
  inputs,
  config,
  pkgs,
  ...
}: let
  inherit (inputs) nixpkgs nixos-generators;
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [
    nixos-generators.nixosModules.install-iso
  ];

  nix = {
    registry.nixpkgs.flake.outPath = l.path {
      name = "source";
      path = pkgs.path;
    };
    package = nixpkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes recursive-nix
    '';
  };

  networking.domain = "local";

  # Provide networkmanager for easy wireless configuration.
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";
  networking.wireless.enable = l.mkForce false;
  networking.wireless.iwd.enable = true;
  services.getty.helpLine =
    ''
      The "nixos" and "root" accounts have empty passwords.

      An ssh daemon is running. You then must set a password
      for either "root" or "nixos" with `passwd` or add an ssh key
      to /home/nixos/.ssh/authorized_keys be able to login.
    ''
    + l.optionalString config.services.xserver.enable ''

      Type `sudo systemctl start display-manager' to
      start the graphical user interface.
    '';

  isoImage = {
    isoBaseName = "bootstrap-hive-from-queen";
    contents = [
      {
        source = inputs.self;
        target = "/hive/";
      }
    ];
  };

  systemd.network = {
    # https://www.freedesktop.org/software/systemd/man/systemd.network.html
    networks."boostrap-link-local" = {
      matchConfig = {
        Name = "en* wl* ww*";
      };
      networkConfig = {
        Description = "Link-local host bootstrap network";
        MulticastDNS = true;
        LinkLocalAddressing = "ipv6";
        DHCP = "yes";
      };
      address = [
        # fall back well-known link-local for situations where MulticastDNS is not available
        "fe80::47" # 47: n=14 i=9 x=24; n+i+x
      ];
      extraConfig = ''
        # Unique, yet stable. Based off the MAC address.
        IPv6LinkLocalAddressGenerationMode = "eui64"
      '';
    };
  };
}
