collective: {inputs, ...}: let
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  # FIXME: move to guardian
  primaryUser = {
    authorizedKeys = import ../secrets/authorized-keys.nix;
  };

  nixosModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = rakeLeaves ./roles;

  suites = {
    server =
      (with (collective.profiles); [
        networking.common
        networking.tailscale
        networking.ssh-host
      ])
      ++ (with profiles; []);

    tangible =
      (with (collective.profiles); [
        networking.common
        networking.tailscale
      ])
      ++ (with profiles; [
        audio
        bluetooth
        printers-scanners
        networking.wifi
      ]);

    workstation =
      suites.tangible
      ++ (with collective.profiles; [
        fonts.common
        fonts.pragmatapro
        networking.ssh-host
        secrets
      ])
      ++ (with profiles; [
        boot.systemd-boot
        gnome-desktop
        video
        workstations.common
        yubikey
        zoom-us
      ]);

    opsbox = with profiles; [
      virtualisation.libvirtd
      virtualisation.podman
      virtualisation.vagrant
      virtualisation.virtualbox
    ];
  };
in {
  imports = [(importHosts ./machines)];

  hostDefaults = {
    system = x86_64-linux;
    channelName = "nixos-unstable";
    # FIXME: intention behind `imports` and `modules` is not clear -- couldn't
    # the `import`ed modules just be imported to `modules`?
    imports = [collective.modules nixosModules];
    modules = [
      collective.profiles.core
      profiles.core
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

  importables = {inherit collective profiles roles suites primaryUser;};
}
