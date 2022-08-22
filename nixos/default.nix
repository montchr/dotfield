collective: {inputs, ...}: let
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  nixosModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit collective profiles;};

  importables = {inherit collective profiles roles primaryUser;};
in {
  inherit importables;

  imports = [(importHosts ./machines)];

  hosts = {
    bootstrap-graphical.modules =
      (with roles; graphical ++ tangible ++ workstation)
      ++ (with profiles; [
        login.gdm
      ]);

    boschic.modules =
      (with roles; graphical ++ tangible ++ webdev ++ workstation)
      ++ (with profiles; [
        boot.refind
        hardware.amd
        login.gdm
        # login.greetd
        nvidia
        virtualisation.vm-variant
        workstations.flatpak
      ]);

    hodgepodge.modules =
      (with roles; graphical ++ tangible ++ workstation)
      ++ (with profiles; [
        hidpi
        login.gdm
        office
      ]);

    hierophant.modules = with profiles; [
      environments.hetzner-cloud
      # TODO: remove, and use the suite or whatever
      # networking.tailscale
    ];

    ryosuke.modules =
      (with roles; graphical ++ tangible ++ webdev ++ workstation)
      ++ (with profiles; [
        boot.systemd-boot
        hardware.amd
        login.gdm
        # login.greetd
        # virtualisation.vm-variant
      ]);

    tsone.modules =
      (with roles; server)
      ++ (with profiles; [hardware.amd]);
  };

  hostDefaults = {
    system = x86_64-linux;
    channelName = "nixos-unstable";
    imports = [
      collective.modules
      nixosModules
    ];
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
}
