{
  self,
  withSystem,
  ops,
  ...
}:
let
  inherit (self) inputs;
  inherit (self.inputs)
    home-manager
    nixos-apple-silicon
    nixos-hardware
    sops-nix
    srvos
    ;

  lib' = self.lib;

  modules = import ./modules-list.nix;

  defaultModules = [
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops

    ./profiles/core/default.nix
    ./profiles/boot/common.nix
    ./profiles/networking/tailscale.nix
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
          flake = lib'.modules.flakeSpecialArgs' system;
        };
        modules =
          defaultModules
          ++ modules
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
  flake.nixosModules = {
    "hardware/keyboard/keyboardio" = import ./modules/hardware/keyboard/keyboardio;
  };

  flake.nixosConfigurations = {
    # bootstrap-graphical = makeNixosSystem "bootstrap-graphical" {
    #   system = x86_64-linux;
    #   modules = with features; desktop ++ gnome ++ workstation;
    # };

    ryosuke = makeNixosSystem "ryosuke" {
      system = "x86_64-linux";
      modules = [
        nixos-hardware.nixosModules.common-cpu-amd
        nixos-hardware.nixosModules.common-cpu-amd-pstate
        nixos-hardware.nixosModules.common-gpu-amd

        ./mixins/gnome.nix
        ./mixins/workstation.nix
        ./mixins/webdev.nix

        ./profiles/hardware/razer.nix
      ];
    };

    tuvok = makeNixosSystem "tuvok" (
      let
        system = "aarch64-linux";
      in
      {
        inherit system;
        # XXX: <https://github.com/NixOS/nixpkgs/pull/317292>
        pkgs = import inputs.nixos-unstable-tuvok {
          inherit system;
          config.allowUnfree = true;
          overlays = [ nixos-apple-silicon.overlays.default ];
        };
        modules = [
          ./mixins/gnome.nix
          ./mixins/workstation.nix

          ./profiles/hardware/apple/macbook-14-2.nix
          ./profiles/remote-builders/nixbuild-net.nix
          ./profiles/virtualisation/ddev.nix
        ];
      }
    );

    moraine = makeNixosSystem "moraine" {
      system = "x86_64-linux";
      modules = [
        ./mixins/server.nix

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
      modules = [
        ./mixins/gnome.nix
        ./mixins/webdev.nix
        ./mixins/workstation.nix

        # FIXME: clarify that this means an amd cpu, NOT gpu
        ./profiles/hardware/amd.nix

        ./profiles/hardware/focusrite-scarlett-18i20-mk1.nix

        # TODO: rename to note that this is gpu, making it mutually exclusive
        #       with an AMD GPU (same goes for intel/amd cpu but i don't bother
        #       with intel cpus)
        ./profiles/hardware/nvidia/stable-release.nix
        ./profiles/hardware/razer.nix
      ];
    };

    hodgepodge = makeNixosSystem "hodgepodge" {
      system = "x86_64-linux";
      modules = [
        ./mixins/gnome.nix
        ./mixins/workstation.nix

        ./profiles/hardware/apple/macbookpro-11-3.nix
      ];
    };

    chert = makeNixosSystem "chert" {
      channel = "stable";
      system = "x86_64-linux";
      modules = [
        ./mixins/server.nix

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
      modules = [
        ./mixins/server.nix

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
      modules = [
        ./mixins/server.nix

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
