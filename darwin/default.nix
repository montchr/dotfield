collective: {inputs, ...}: let
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin x86_64-darwin;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  darwinModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit collective profiles;};

  importables = {inherit collective profiles roles primaryUser;};
in {
  inherit importables;

  imports = [(importHosts ./machines)];

  hosts.cdotmp = {
    system = x86_64-darwin;
  };

  hostDefaults = {
    system = aarch64-darwin;
    channelName = "nixos-unstable";
    imports = [collective.modules darwinModules];
    modules = [
      collective.profiles.core
      profiles.core
      home-manager.darwinModules.home-manager
      # `nixosModules` is correct, even for darwin
      # FIXME: migrate to sops
      agenix.nixosModules.age
    ];
  };
}
