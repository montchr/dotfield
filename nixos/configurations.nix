{
  withSystem,
  collective,
  self,
  lib,
  ...
}: let
  inherit
    (self)
    inputs
    nixpkgsConfig
    sharedModules
    sharedProfiles
    ;
  inherit
    (inputs.digga.lib)
    flattenTree
    rakeLeaves
    ;
  inherit
    (inputs)
    nixpkgs
    nixpkgs-wayland
    nixos-stable
    nixos-unstable
    agenix
    home-manager
    nur
    sops-nix
    ;
  inherit
    (nixpkgs.lib)
    mapAttrs
    mapAttrs'
    makeOverridable
    nixosSystem
    ;
  inherit
    (inputs.flake-utils.lib.system)
    x86_64-linux
    ;
  inherit
    (self.nixosModules)
    hm-shared-config
    ;

  roles = import ./roles {inherit sharedProfiles nixosProfiles;};

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  nixosModules = rakeLeaves ./modules;
  nixosMachines = rakeLeaves ./machines;
  nixosProfiles = rakeLeaves ./profiles;

  defaultModules = [
    {
      imports =
        (builtins.attrValues (flattenTree nixosModules))
        ++ [
          ({pkgs, ...}: {
            nix.nixPath = [
              "nixpkgs=${pkgs.path}"
              "home-manager=${home-manager}"
            ];
            documentation.info.enable = false;
          })

          sharedProfiles.core
          sharedProfiles.homeManagerSettings

          nixosProfiles.core
          nixosProfiles.boot.common

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
    withSystem system (
      ctx @ {
        pkgs,
        inputs',
        sources,
        ...
      }: let
        moduleArgs = {
          _module.args.inputs = self.inputs;
          _module.args.inputs' = inputs';
          _module.args.primaryUser = primaryUser;
          _module.args.packages = ctx.config.packages;
          _module.args.sources = sources;
        };
      in
        makeOverridable nixosSystem {
          inherit system;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ modules
            ++ [
              moduleArgs
              {home-manager.sharedModules = [moduleArgs];}
              nixosMachines.${hostname}
            ];
          specialArgs = {
            inherit
              self
              inputs
              sources
              nixosProfiles
              sharedProfiles
              roles
              ;
          };
        }
    );
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
