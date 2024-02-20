# FIXME: no more haumea-compiled profiles/suites! import directly and simply!
{
  self,
  withSystem,
  ops,
  ...
}: let
  inherit (self) inputs lib;
  inherit (inputs) apparat haumea home-manager nixos-apple-silicon nixpkgs srvos;
  inherit (apparat.lib) flattenTree;
  l = inputs.nixpkgs.lib // builtins;

  nixosModules = import ../modules/nixosModules.nix;
  nixosProfiles = import ../profiles/nixosProfiles.nix {inherit haumea;};
  nixosSuites = import ../profiles/nixosSuites.nix {inherit sharedProfiles nixosProfiles;};
  sharedModules = import ../modules/sharedModules.nix {inherit haumea;};
  sharedProfiles = import ../profiles/sharedProfiles.nix {inherit haumea;};

  nixosModules' = flattenTree nixosModules;
  sharedModules' = flattenTree sharedModules;

  defaultModules = [
    sharedProfiles.core.default
    sharedProfiles.secrets.default
    nixosProfiles.core.common
    nixosProfiles.boot.common
    nixosProfiles.networking.tailscale
    home-manager.nixosModules.home-manager
  ];

  makeNixosSystem = hostName: nixosArgs @ {system, ...}:
    withSystem system (
      {pkgs, ...}:
        l.nixosSystem {
          inherit system;
          specialArgs = {
            inherit nixosProfiles sharedProfiles;
            flake = lib.modules.flakeSpecialArgs' system;
          };
          modules =
            defaultModules
            ++ (l.attrValues sharedModules')
            ++ (l.attrValues nixosModules')
            ++ (nixosArgs.modules or [])
            ++ [
              ./${hostName}
              {
                _module.args = {inherit ops;};
                nixpkgs.pkgs = nixosArgs.pkgs or pkgs;
                networking.hostName = hostName;
              }
            ];
        }
    );
in {
  flake.nixosModules = nixosModules;
  flake.nixosConfigurations = {
    # bootstrap-graphical = makeNixosSystem "bootstrap-graphical" {
    #   system = x86_64-linux;
    #   modules = with nixosSuites; desktop ++ gnome ++ workstation;
    # };

    freundix = makeNixosSystem "freundix" {
      system = "x86_64-linux";
      modules = with nixosSuites; gnome ++ graphical;
    };

    ryosuke = makeNixosSystem "ryosuke" {
      system = "x86_64-linux";
      modules =
        (with nixosSuites; desktop ++ gnome ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          hardware.amd
          # login.greetd
          # virtualisation.vm-variant
        ]);
    };

    tuvok = makeNixosSystem "tuvok" (let
      system = "aarch64-linux";
    in {
      inherit system;
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          nixos-apple-silicon.overlays.default
          (final: prev: {
            fd = self.packages.${final.stdenv.system}.fd;
          })
        ];
      };
      modules =
        nixosSuites.gnome
        ++ nixosSuites.desktop
        ++ nixosSuites.workstation
        ++ [
          nixosProfiles.hardware.asahi
          nixosProfiles.hardware.laptop
        ];
    });

    moraine = makeNixosSystem "moraine" {
      system = "x86_64-linux";
      modules =
        nixosSuites.server
        ++ [
          srvos.nixosModules.server
          srvos.nixosModules.hardware-hetzner-online-amd
          srvos.nixosModules.mixins-nginx
          srvos.nixosModules.mixins-terminfo
          srvos.nixosModules.mixins-tracing
          srvos.nixosModules.mixins-trusted-nix-caches
          # TODO: needs additional config
          srvos.nixosModules.mixins-telegraf

          # FIXME: needs security before enable
          # nixosProfiles.monitoring.prometheus
          # nixosProfiles.monitoring.telegraf
        ];
    };

    boschic = makeNixosSystem "boschic" {
      system = "x86_64-linux";
      modules =
        (with nixosSuites; gnome ++ desktop ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          boot.refind
          desktop.flatpak
          # FIXME: clarify that this means an amd cpu, NOT gpu
          hardware.amd
          # login.greetd
          # TODO: rename to note that this is gpu, making it mutually exclusive with an AMD GPU
          #       (same goes for intel/amd cpu but i don't bother with intel cpus)
          hardware.nvidia
          # FIXME: remove
          # virtualisation.vm-variant
        ]);
    };

    hodgepodge = makeNixosSystem "hodgepodge" {
      system = "x86_64-linux";
      modules =
        nixosSuites.gnome
        ++ nixosSuites.desktop
        ++ nixosSuites.workstation
        ++ [
          nixosProfiles.hardware.macbookpro-11-3
          nixosProfiles.virtualisation.quickemu
        ];
    };

    chert = makeNixosSystem "chert" {
      system = "x86_64-linux";
      modules =
        nixosSuites.server
        ++ [
          # TODO: verify whether these conflict with operations, esp. non-mutable users?
          # srvos.nixosModules.server
          # srvos.nixosModules.mixins-nginx
          srvos.nixosModules.hardware-hetzner-cloud
          srvos.nixosModules.mixins-terminfo
          srvos.nixosModules.mixins-tracing
          srvos.nixosModules.mixins-trusted-nix-caches
          # TODO: needs additional config
          srvos.nixosModules.mixins-telegraf
        ];
    };

    gabbro = makeNixosSystem "gabbro" {
      system = "x86_64-linux";
      modules =
        nixosSuites.server
        ++ [
          # TODO: verify whether these conflict with operations, esp. non-mutable users?
          # srvos.nixosModules.server
          # srvos.nixosModules.mixins-nginx
          srvos.nixosModules.hardware-hetzner-cloud
          srvos.nixosModules.mixins-terminfo
          srvos.nixosModules.mixins-tracing
          srvos.nixosModules.mixins-trusted-nix-caches
          # TODO: needs additional config
          srvos.nixosModules.mixins-telegraf
        ];
    };

    hierophant = makeNixosSystem "hierophant" {
      system = "x86_64-linux";
      modules =
        nixosSuites.server
        ++ [
          srvos.nixosModules.server
          srvos.nixosModules.hardware-hetzner-cloud
          srvos.nixosModules.mixins-nginx
          srvos.nixosModules.mixins-terminfo
          srvos.nixosModules.mixins-tracing
          srvos.nixosModules.mixins-trusted-nix-caches
          # TODO: needs additional config
          srvos.nixosModules.mixins-telegraf
        ];
    };
  };
}
