{
  withSystem,
  self,
  lib,
  peers,
  ...
}: let
  inherit
    (self)
    inputs
    sharedModules
    sharedProfiles
    ;
  inherit (self.nixosModules) homeManagerSettings;
  inherit
    (inputs.digga.lib)
    flattenTree
    rakeLeaves
    ;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit
    (lib)
    makeOverridable
    nixosSystem
    ;

  roles = import ./roles {inherit sharedProfiles nixosProfiles;};

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  nixosModules = rakeLeaves ./modules;
  nixosMachines = rakeLeaves ./machines;
  nixosProfiles = rakeLeaves ./profiles;

  defaultModules = [
    sharedProfiles.core
    homeManagerSettings
    nixosProfiles.core
    nixosProfiles.boot.common
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.agenix.nixosModules.age
  ];

  makeNixosSystem = hostname: nixosArgs:
    withSystem (nixosArgs.system or x86_64-linux) (
      ctx @ {system, ...}: let
        pkgs = nixosArgs.pkgs or ctx.pkgs;
        moduleArgs = {
          _module.args.self = self;
          _module.args.inputs = self.inputs;
          _module.args.primaryUser = primaryUser;
          _module.args.packages = ctx.config.packages;
          _module.args.sources = ctx.sources;
          _module.args.peers = peers;
        };
      in
        makeOverridable nixosSystem {
          inherit system;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ (builtins.attrValues (flattenTree nixosModules))
            ++ (nixosArgs.modules or [])
            ++ [
              nixosMachines.${hostname}
              moduleArgs
              {
                nixpkgs.pkgs = pkgs;
                networking.hostName = hostname;
                home-manager.sharedModules = [moduleArgs];
                # if this were enabled, rebuilds will take forrrreevvvveerrrrr.
                documentation.info.enable = false;
              }
            ];
          specialArgs = {
            inherit
              nixosProfiles
              sharedProfiles
              roles
              system
              ;
            inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux isMacOS;
          };
        }
    );
in {
  flake.nixosModules = nixosModules;
  flake.nixosConfigurations = {
    bootstrap-graphical = makeNixosSystem "bootstrap-graphical" {
      modules =
        (with roles; graphical ++ tangible ++ workstation)
        ++ (with nixosProfiles; [login.gdm]);
    };

    ryosuke = makeNixosSystem "ryosuke" {
      modules =
        (with roles; graphical ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          hardware.amd
          login.gdm
          # login.greetd
          # virtualisation.vm-variant
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
          # FIXME: `lib.mkForce` fails
          # hardware.nvidia
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
