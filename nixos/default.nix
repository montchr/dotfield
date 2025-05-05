{
  self,
  withSystem,
  ops,
  ...
}:
let
  inherit (self) inputs;
  inherit (self.inputs)
    disko
    home-manager
    nixos-apple-silicon
    nixos-generators
    nixos-hardware
    sops-nix
    srvos
    stylix
    ;

  lib' = self.lib;

  modules = import ./modules-list.nix;

  defaultModules = [
    home-manager.nixosModules.home-manager
    sops-nix.nixosModules.sops
    stylix.nixosModules.stylix

    ./profiles/core/default.nix
    ./profiles/boot/common.nix
    ./profiles/networking/tailscale.nix
  ];

  makeAsahiPkgs =
    {
      channel ? "nixpkgs-apple-silicon",
      buildPlatform ? "aarch64-linux",
    }:
    import inputs.${channel} {
      config.allowUnfree = true;
      crossSystem.system = "aarch64-linux";
      localSystem.system = buildPlatform;
      overlays = (import ../overlays/default.nix { inherit inputs; }) ++ [
        inputs.nixos-apple-silicon.overlays.default
      ];
    };

  makeAsahiSystem =
    hostName: nixosArgs:
    let
      system = "aarch64-linux";
      channel = "nixpkgs-apple-silicon";
    in
    inputs.${channel}.lib.nixosSystem {
      inherit system;
      specialArgs = {
        flake = lib'.modules.flakeSpecialArgs' system;
      };
      modules =
        defaultModules
        ++ modules
        ++ (nixosArgs.modules or [ ])
        ++ [
          inputs.nixos-apple-silicon.nixosModules.apple-silicon-support

          ../machines/${hostName}

          {
            _module.args = {
              inherit ops;
            };
            nixpkgs.pkgs = makeAsahiPkgs { inherit channel; };
            networking.hostName = hostName;
          }
        ];
    };

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

  generateSystemArtifact =
    hostName:
    args@{
      format,
      system,
      channel ? "unstable",
      ...
    }:
    withSystem system (
      { pkgs, ... }:
      inputs.nixos-generators.nixosGenerate {
        inherit system format;
        pkgs = args.pkgs or pkgs;
        specialArgs = {
          flake = lib'.modules.flakeSpecialArgs' system;
        };
        modules =
          defaultModules
          ++ modules
          ++ (args.modules or [ ])
          ++ [
            ../machines/${hostName}
            {
              _module.args = {
                inherit ops;
              };
              networking.hostName = hostName;
            }
          ];
      }
    );

  makeAsahiInstallerPackage =
    localSystem: targetHostName:
    let
      installer = makeNixosSystem targetHostName {
        system = "aarch64-linux";
        pkgs = makeAsahiPkgs localSystem { };
        modules = [
          "${nixos-apple-silicon}/iso-configuration"
          ./mixins/installer.nix
          ./profiles/hardware/apple/apple-silicon.nix
          { hardware.asahi.pkgsSystem = localSystem; }
        ];
      };
    in
    (installer.config.system.build.isoImage.overrideAttrs (o: {
      # add ability to access the whole config from the command line
      passthru = (o.passthru or { }) // {
        inherit (installer) config;
      };
    }));
in
{
  perSystem =
    { system, ... }:
    {
      packages = {
        tuuvok-installer-iso = makeAsahiInstallerPackage system "tuuvok-installer";
      };
    };

  flake.nixosModules = {
    "hardware/keyboard/keyboardio" = import ./modules/hardware/keyboard/keyboardio;
  };

  flake.nixosConfigurations = {
    ryosuke = makeNixosSystem "ryosuke" {
      system = "x86_64-linux";
      modules = [
        nixos-hardware.nixosModules.common-cpu-amd
        nixos-hardware.nixosModules.common-cpu-amd-pstate
        nixos-hardware.nixosModules.common-gpu-amd

        ./mixins/gnome.nix
        ./mixins/jobwork.nix
        ./mixins/workstation.nix

        ./profiles/hardware/razer.nix
        ./profiles/remote-builders/default.nix
      ];
    };

    tuuvok = makeAsahiSystem "tuuvok" {
      modules = [
        ./mixins/jobwork.nix
        ./mixins/hyprland.nix
        ./mixins/sway.nix
        ./mixins/workstation.nix

        ./profiles/hardware/apple/macbook-14-2/default.nix
        ./profiles/hardware/displaylink.nix

        ./profiles/remote-builders/default.nix
        ./profiles/remote-builders/ryosuke.nix
      ];
    };

    platauc = makeNixosSystem "platauc" {
      system = "aarch64-linux";
      modules = [
        disko.nixosModules.disko

        srvos.nixosModules.roles-nix-remote-builder
        srvos.nixosModules.hardware-hetzner-cloud-arm

        srvos.nixosModules.mixins-terminfo
        srvos.nixosModules.mixins-tracing
        srvos.nixosModules.mixins-trusted-nix-caches
        srvos.nixosModules.server
      ];
    };

    boschic = makeNixosSystem "boschic" {
      system = "x86_64-linux";
      modules = [
        ./mixins/gnome.nix
        ./mixins/jobwork.nix
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
