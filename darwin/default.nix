collective: {inputs, ...}: let
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin x86_64-darwin;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  darwinModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = rakeLeaves ./roles;
in {
  imports = [(importHosts ./machines)];
  importables = {inherit collective profiles roles;};
  hostDefaults = {
    system = aarch64-darwin;
    channelName = "nixos-unstable";
    # FIXME: intention behind `imports` and `modules` is not clear -- couldn't
    # the `import`ed modules just be imported to `modules`?
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

  hosts.cdotmp = {
    system = x86_64-darwin;
  };
}
