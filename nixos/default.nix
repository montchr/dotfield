args: {inputs, ...}: let
  inherit (args) peers;
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  nixosModules = importExportableModules ./modules;
  profiles = {
    shared = args.profiles;
    # TODO: after restructuring profiles per-host-type, remove this namespacing
    system = rakeLeaves ./profiles;
  };
  roles = rakeLeaves ./roles;
in {
  imports = [(importHosts ./machines)];

  hostDefaults = {
    system = x86_64-linux;
    channelName = "nixos-unstable";
    # FIXME: intention behind `imports` and `modules` is not clear -- couldn't
    # the `import`ed modules just be imported to `modules`?
    imports = [args.modules nixosModules];
    modules = [
      roles.common
      home-manager.nixosModules.home-manager

      # FIXME: upstream module causes a huge number of unnecessary
      # dependencies to be pulled in for all systems -- many of them are
      # graphical. should only be imported as needed.
      # digga.nixosModules.bootstrapIso

      # FIXME: upstream module needs updating for unstable
      # digga.nixosModules.nixConfig

      # FIXME: migrate to sops
      agenix.nixosModules.age
    ];
  };

  hosts = {
    boschic = {};
    hodgepodge = {};
    hierophant = {};
    ryosuke = {};
    tsone = {};
    bootstrap-graphical = {};
  };

  importables = rec {
    inherit peers profiles roles;

    # FIXME: move to guardian
    primaryUser = {
      authorizedKeys = import ../secrets/authorized-keys.nix;
    };

    suites = with (profiles.shared); rec {
      server = [
        networking.common
        networking.tailscale
        ssh-host
      ];

      tangible = [
        audio
        bluetooth
        networking.common
        networking.tailscale
        networking.wifi
        printers-scanners
      ];

      workstation =
        tangible
        ++ [
          boot.systemd-boot
          fonts.common
          fonts.pragmatapro
          gnome-desktop
          secrets
          video
          workstations.common
          yubikey
          zoom-us
        ];

      opsbox = [
        virtualisation.libvirtd
        virtualisation.podman
        virtualisation.vagrant
        virtualisation.virtualbox
      ];
    };
  };
}
