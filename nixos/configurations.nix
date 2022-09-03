{
  withSystem,
  collective,
  self,
  lib,
  ...
}: let
  inherit (self) inputs sharedProfiles;
  inherit (lib) mapAttrs;
  inherit (lib.dotfield) importLeaves;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit (inputs.digga.lib) flattenTree rakeLeaves;

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  nixosMachines = rakeLeaves ./machines;
  nixosModules = rakeLeaves ./modules;
  nixosProfiles = rakeLeaves ./profiles;

  roles = import ./roles {inherit sharedProfiles nixosProfiles;};

  defaultModules =
    (builtins.attrValues self.nixosModules)
    ++ [
      {
        _module.args = {
          inherit
            self
            inputs
            collective
            nixosProfiles
            primaryUser
            ;
        };
      }

      sharedProfiles.core
      nixosProfiles.core
      nixosProfiles.boot.common

      inputs.home-manager.nixosModules.home-manager
      # FIXME: migrate to sops
      inputs.agenix.nixosModules.age
    ];

  makeNixosSystem = hostname: {
    system ? x86_64-linux,
    modules ? [],
    extraModuleArgs ? {},
  }: (withSystem system (ctx @ {pkgs, ...}: (
    pkgs.nixos ({...}: {
      imports =
        defaultModules
        ++ modules
        ++ [
          ({_module.args.packages = ctx.config.packages;} // extraModuleArgs)
          nixosMachines.${hostname}
        ];
    })
  )));
in {
  flake.nixosModules = importLeaves nixosModules;
  flake.nixosProfiles = importLeaves nixosProfiles;
  flake.nixosConfigurations = {
    bootstrap-graphical = makeNixosSystem "bootstrap-graphical" {
      modules =
        (with roles; graphical ++ tangible ++ workstation)
        ++ (with nixosProfiles; [
          login.gdm
        ]);
    };

    boschic = makeNixosSystem "boschic" {
      modules =
        (with roles; graphical ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          boot.refind
          desktop.flatpak
          hardware.amd
          login.gdm
          # login.greetd
          hardware.nvidia
          virtualisation.vm-variant
        ]);
    };

    hodgepodge = makeNixosSystem "hodgepodge" {
      modules =
        (with roles; graphical ++ tangible ++ workstation)
        ++ (with nixosProfiles; [
          hardware.hidpi
          login.gdm
        ]);
    };

    hierophant = makeNixosSystem "hierophant" {
      modules =
        (with roles; server)
        ++ (with nixosProfiles; [
          environments.hetzner-cloud
        ]);
    };

    ryosuke = makeNixosSystem "ryosuke" {
      modules =
        (with roles; graphical ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          hardware.amd
          hardware.hidpi
          login.gdm
          # login.greetd
          # virtualisation.vm-variant
        ]);
    };

    tsone = makeNixosSystem "tsone" {
      modules =
        (with roles; server)
        ++ (with nixosProfiles; [
          hardware.amd

          # This host has Hetzner Online UEFI boot enabled in BIOS.
          # Normally Hetzner Online would require booting with GRUB.
          boot.systemd-boot
        ]);
    };
  };
}
