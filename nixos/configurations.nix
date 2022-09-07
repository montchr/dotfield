{
  withSystem,
  collective,
  self,
  ...
}: let
  inherit (self) inputs sharedModules sharedProfiles;
  inherit (nixpkgs.lib) mapAttrs mapAttrs';
  inherit (inputs.digga.lib) flattenTree rakeLeaves;

  inherit
    (inputs)
    nixpkgs
    nixpkgs-wayland
    nixos-stable
    nixos-unstable
    agenix
    home-manager
    nur
    nvfetcher
    sops-nix
    ;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit (self.nixosModules) hm-shared-config;

  inherit
    (import ../nixpkgs-config.nix {inherit self;})
    overlays
    packageOverrides
    ;

  roles = import ./roles {inherit sharedProfiles nixosProfiles;};

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  nixosModules = rakeLeaves ./modules;
  nixosMachines = rakeLeaves ./machines;
  nixosProfiles = rakeLeaves ./profiles;

  defaultModules = [
    {
      _module.args.self = self;
      _module.args.inputs = self.inputs;
      _module.args.primaryUser = primaryUser;
    }
    {
      imports =
        (builtins.attrValues (flattenTree nixosModules))
        ++ [
          # TODO: this may not work...
          # ({pkgs, ...}: {
          #   _module.args.packages = self.config.packages;
          # })
          #

          ({pkgs, ...}: {
            nix.nixPath =
              # (lib.optionals pkgs.stdenv.hostPlatform.isDarwin "darwin=${darwin}")
              # ++ [
              [
                "nixpkgs=${pkgs.path}"
                "home-manager=${home-manager}"
              ];
            documentation.info.enable = false;
            nixpkgs = {
              inherit overlays;
              config = {
                inherit packageOverrides;
                allowUnfree = true;
              };
            };
          })

          sharedProfiles.core
          nixosProfiles.core
          nixosProfiles.boot.common

          hm-shared-config

          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
          agenix.nixosModules.age
        ];
    }
  ];

  makeNixosSystem = hostname: {
    system ? x86_64-linux,
    modules ? [],
  }:
    nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
      inherit system;
      modules =
        defaultModules
        ++ (builtins.attrValues sharedModules)
        ++ modules
        ++ [
          nixosMachines.${hostname}
        ];
      specialArgs = {inherit collective nixosProfiles roles;};
    };
in {
  # flake.nixosModules = nixosModules;
  flake.nixosConfigurations = {
    bootstrap-graphical = makeNixosSystem "bootstrap-graphical" {
      modules =
        (with roles; graphical ++ tangible ++ workstation)
        ++ (with nixosProfiles; [
          login.gdm
        ]);
    };

    # boschic = makeNixosSystem "boschic" {
    #   modules =
    #     (with roles; graphical ++ tangible ++ webdev ++ workstation)
    #     ++ (with nixosProfiles; [
    #       boot.refind
    #       desktop.flatpak
    #       hardware.amd
    #       login.gdm
    #       # login.greetd
    #       hardware.nvidia
    #       virtualisation.vm-variant
    #     ]);
    # };

    # hodgepodge = makeNixosSystem "hodgepodge" {
    #   modules =
    #     (with roles; graphical ++ tangible ++ workstation)
    #     ++ (with nixosProfiles; [
    #       hardware.hidpi
    #       login.gdm
    #     ]);
    # };

    # hierophant = makeNixosSystem "hierophant" {
    #   modules =
    #     (with roles; server)
    #     ++ (with nixosProfiles; [
    #       environments.hetzner-cloud
    #     ]);
    # };

    # ryosuke = makeNixosSystem "ryosuke" {
    #   modules =
    #     (with roles; graphical ++ tangible ++ webdev ++ workstation)
    #     ++ (with nixosProfiles; [
    #       hardware.amd
    #       hardware.hidpi
    #       login.gdm
    #       # login.greetd
    #       # virtualisation.vm-variant
    #     ]);
    # };

    # tsone = makeNixosSystem "tsone" {
    #   modules =
    #     (with roles; server)
    #     ++ (with nixosProfiles; [
    #       hardware.amd

    #       # This host has Hetzner Online UEFI boot enabled in BIOS.
    #       # Normally Hetzner Online would require booting with GRUB.
    #       boot.systemd-boot
    #     ]);
    # };
  };
}
