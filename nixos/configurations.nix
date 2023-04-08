{
  withSystem,
  self,
  # DEPRECATED:
  peers,
  primaryUser,
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
  l = inputs.nixpkgs.lib // builtins;

  roles = import ./roles {inherit sharedProfiles nixosProfiles;};

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

  makeNixosSystem = hostname: nixosArgs @ {system, ...}:
    withSystem system (
      {
        inputs',
        cells,
        pkgs,
        packages,
        ...
      }:
        l.makeOverridable l.nixosSystem {
          inherit system;
          modules =
            defaultModules
            ++ (l.attrValues sharedModules)
            ++ (l.attrValues (flattenTree nixosModules))
            ++ (nixosArgs.modules or [])
            ++ [
              nixosMachines.${hostname}
              {
                _module.args = {
                  inherit
                    cells
                    inputs'
                    packages
                    peers
                    primaryUser
                    ;
                  isNixos = true;
                };
                nixpkgs.pkgs = nixosArgs.pkgs or pkgs;
                networking.hostName = hostname;
                home-manager.sharedModules = [{_module.args.isNixos = true;}];
              }
            ];
          specialArgs = {
            inherit
              self
              inputs
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
      system = x86_64-linux;
      modules = with roles; gnome ++ graphical ++ tangible ++ workstation;
    };

    freundix = makeNixosSystem "freundix" {
      system = x86_64-linux;
      modules = with roles; gnome ++ graphical;
    };

    ryosuke = makeNixosSystem "ryosuke" {
      system = x86_64-linux;
      modules =
        (with roles; gnome ++ graphical ++ office ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          hardware.amd
          # login.greetd
          # virtualisation.vm-variant
        ]);
    };

    boschic = makeNixosSystem "boschic" {
      system = x86_64-linux;
      modules =
        (with roles; gnome ++ graphical ++ office ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          boot.refind
          desktop.flatpak
          hardware.amd
          # login.greetd
          # FIXME: `lib.mkForce` fails
          # hardware.nvidia
          virtualisation.vm-variant
        ]);
    };

    hodgepodge = makeNixosSystem "hodgepodge" {
      system = x86_64-linux;
      modules = with roles; gnome ++ graphical ++ office ++ tangible ++ workstation;
    };

    hierophant = makeNixosSystem "hierophant" {
      system = x86_64-linux;
      modules =
        (with roles; server)
        ++ (with nixosProfiles; [
          environments.hetzner-cloud
        ]);
    };

    tsone = makeNixosSystem "tsone" {
      system = x86_64-linux;
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
