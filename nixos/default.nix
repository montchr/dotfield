{
  self,
  withSystem,
  ops,
  ...
}:
let
  inherit (self) inputs;
  inherit (self.inputs)
    haumea
    home-manager
    kmonad
    nixos-apple-silicon
    nixpkgs
    srvos
    ;

  lib' = self.lib;

  commonModules = import ../common/modules-list.nix;
  nixosModules = import ../nixos/modules-list.nix;

  sharedProfiles = import ../common/profiles.nix { inherit haumea; };
  profiles = import ./profiles.nix { inherit haumea; };

  features = import ./features.nix { inherit sharedProfiles profiles; };

  defaultModules = [
    sharedProfiles.core.default
    sharedProfiles.secrets.default
    profiles.core.common
    profiles.boot.common
    profiles.networking.tailscale
    home-manager.nixosModules.home-manager
    kmonad.nixosModules.default
  ];

  makeNixosSystem =
    hostName:
    nixosArgs@{
      channel ? "unstable",
      system,
      ...
    }:
    withSystem system (
      { pkgs, ... }:
      inputs."nixos-${channel}".lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit sharedProfiles profiles;
          flake = lib'.modules.flakeSpecialArgs' system;
        };
        modules =
          defaultModules
          ++ commonModules
          ++ nixosModules
          ++ (nixosArgs.modules or [ ])
          ++ [
            ../machines/${hostName}
            {
              _module.args = {
                inherit ops;
              };
              nixpkgs.pkgs = nixosArgs.pkgs or pkgs;
              networking.hostName = hostName;
            }
          ];
      }
    );
in
{
  flake.nixosModules = nixosModules;

  flake.nixosConfigurations = {
    # bootstrap-graphical = makeNixosSystem "bootstrap-graphical" {
    #   system = x86_64-linux;
    #   modules = with features; desktop ++ gnome ++ workstation;
    # };

    ryosuke = makeNixosSystem "ryosuke" {
      system = "x86_64-linux";
      modules =
        (with features; desktop ++ gnome ++ webdev ++ workstation)
        ++ (with profiles; [
          hardware.amd
          hardware.razer
          # login.greetd
          # virtualisation.vm-variant
        ]);
    };

    tuvok = makeNixosSystem "tuvok" (
      let
        system = "aarch64-linux";
      in
      {
        inherit system;
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ nixos-apple-silicon.overlays.default ];
        };
        modules =
          features.gnome
          ++ features.desktop
          ++ features.workstation
          ++ [ profiles.hardware.apple.macbook-14-2 ];
      }
    );

    moraine = makeNixosSystem "moraine" {
      system = "x86_64-linux";
      modules = features.server ++ [
        srvos.nixosModules.server
        srvos.nixosModules.hardware-hetzner-online-amd
        srvos.nixosModules.mixins-nginx
        srvos.nixosModules.mixins-terminfo
        srvos.nixosModules.mixins-tracing
        srvos.nixosModules.mixins-trusted-nix-caches
        # TODO: needs additional config
        srvos.nixosModules.mixins-telegraf

        # FIXME: needs security before enable
        # profiles.monitoring.prometheus
        # profiles.monitoring.telegraf
      ];
    };

    boschic = makeNixosSystem "boschic" {
      system = "x86_64-linux";
      modules =
        (with features; gnome ++ desktop ++ webdev ++ workstation)
        ++ (with profiles; [
          boot.refind
          desktop.flatpak
          # FIXME: clarify that this means an amd cpu, NOT gpu
          hardware.amd
          hardware.focusrite-scarlett-18i20-mk1
          # TODO: rename to note that this is gpu, making it mutually exclusive with an AMD GPU
          #       (same goes for intel/amd cpu but i don't bother with intel cpus)
          hardware.nvidia.stable-release
          hardware.razer
        ]);
    };

    hodgepodge = makeNixosSystem "hodgepodge" {
      system = "x86_64-linux";
      modules =
        features.gnome
        ++ features.desktop
        ++ features.workstation
        ++ [
          profiles.hardware.apple.macbookpro-11-3
          profiles.virtualisation.quickemu
        ];
    };

    chert = makeNixosSystem "chert" {
      system = "x86_64-linux";
      modules = features.server ++ [
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
      modules = features.server ++ [
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
      modules = features.server ++ [
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
