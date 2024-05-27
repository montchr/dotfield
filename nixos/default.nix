{
  self,
  inputs,
  lib,
  flake-parts-lib,
  ...
}:
let
  inherit (inputs)
    home-manager
    nixos-apple-silicon
    sops-nix
    srvos
    ;
  inherit (flake-parts-lib) importApply;

  features = import ./features.nix;

  initSystemModule =
    { hostName }:
    {
      imports = [ ../machines/${hostName} ];
      networking.hostName = hostName;
    };

  initNixpkgsModule = importApply ../packages/nixpkgs-config.nix;

  makeNixosSystem =
    hostName: system:
    {
      channel ? "unstable",
      modules ? [ ],
      overlays ? [ ],
      allowUnfree ? false,
    }:
    let
      channelInput = "nixos-${channel}";
      specialArgs = self.lib.specialArgsFor system;
    in
    inputs.${channelInput}.lib.nixosSystem {
      inherit system specialArgs;
      modules =
        modules
        ++ (import ./modules-list.nix)
        ++ (import ./baseline.nix)
        ++ [
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops

          (initSystemModule { inherit hostName; })
          (initNixpkgsModule { inherit allowUnfree overlays; })

          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              sharedModules = (import ../home/modules-list.nix) ++ (import ../home/baseline.nix);
              extraSpecialArgs = specialArgs;
            };
          }
        ];
    };
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

    ryosuke = makeNixosSystem "ryosuke" "x86_64-linux" {
      modules = (with features; desktop ++ gnome ++ webdev ++ workstation) ++ [
        ./profiles/hardware/amd.nix
        ./profiles/hardware/razer.nix
      ];
    };

    tuvok = makeNixosSystem "tuvok" "aarch64-linux" ({
      overlays = [ nixos-apple-silicon.overlays.default ];
      modules =
        features.gnome
        ++ features.desktop
        ++ features.workstation
        ++ [ ./profiles/hardware/apple/macbook-14-2.nix ];
    });

    moraine = makeNixosSystem "moraine" "x86_64-linux" {
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

    boschic = makeNixosSystem "boschic" "x86_64-linux" {
      modules = (with features; gnome ++ desktop ++ webdev ++ workstation) ++ [
        ./profiles/boot/refind.nix
        ./profiles/desktop/flatpak.nix
        # FIXME: clarify that this means an amd cpu, NOT gpu
        ./profiles/hardware/amd.nix
        ./profiles/hardware/focusrite-scarlett-18i20-mk1.nix
        # TODO: rename to note that this is gpu, making it mutually exclusive with an AMD GPU
        #       (same goes for intel/amd cpu but i don't bother with intel cpus)
        ./profiles/hardware/nvidia/stable-release.nix
        ./profiles/hardware/razer.nix
      ];
    };

    hodgepodge = makeNixosSystem "hodgepodge" "x86_64-linux" {
      modules =
        features.gnome
        ++ features.desktop
        ++ features.workstation
        ++ [
          ./profiles/hardware/apple/macbookpro-11-3.nix
          ./profiles/virtualisation/quickemu.nix
        ];
    };

    chert = makeNixosSystem "chert" "x86_64-linux" {
      channel = "stable";
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

    gabbro = makeNixosSystem "gabbro" "x86_64-linux" {
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

    hierophant = makeNixosSystem "hierophant" "x86_64-linux" {
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
