{
  self,
  withSystem,
  ops,
  ...
}: let
  inherit (self) inputs lib;
  inherit (inputs) apparat haumea srvos;
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
    nixosProfiles.core
    nixosProfiles.boot.common
    inputs.agenix.nixosModules.age
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
  ];

  makeNixosSystem = hostName: nixosArgs @ {system, ...}:
    withSystem system (
      {pkgs, ...}:
        l.makeOverridable l.nixosSystem {
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
    #   modules = with nixosSuites; gnome ++ graphical ++ tangible ++ workstation;
    # };

    freundix = makeNixosSystem "freundix" {
      system = "x86_64-linux";
      modules = with nixosSuites; gnome ++ graphical;
    };

    ryosuke = makeNixosSystem "ryosuke" {
      system = "x86_64-linux";
      modules =
        (with nixosSuites; gnome ++ graphical ++ office ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          hardware.amd
          # login.greetd
          # virtualisation.vm-variant
        ]);
    };

    moraine-dev = makeNixosSystem "moraine-dev" {
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
          # srvos.nixosModules.mixins-telegraf
        ];
    };

    boschic = makeNixosSystem "boschic" {
      system = "x86_64-linux";
      modules =
        (with nixosSuites; gnome ++ graphical ++ office ++ tangible ++ webdev ++ workstation)
        ++ (with nixosProfiles; [
          boot.refind
          desktop.flatpak
          hardware.amd
          # login.greetd
          # FIXME: `lib.mkForce` fails
          # hardware.nvidia
          # FIXME: remove
          # virtualisation.vm-variant
        ]);
    };

    hodgepodge = makeNixosSystem "hodgepodge" {
      system = "x86_64-linux";
      modules = with nixosSuites; gnome ++ graphical ++ office ++ tangible ++ workstation;
    };

    hierophant = makeNixosSystem "hierophant" {
      system = "x86_64-linux";
      modules =
        (with nixosSuites; server)
        ++ (with nixosProfiles; [
          # TODO: replace with srvos module
          environments.hetzner-cloud
        ]);
    };
  };
}