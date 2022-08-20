{peers}: {self, ...}: let
  inherit (self.inputs) digga nix-colors;
  inherit (digga.lib) importExportableModules rakeLeaves;

  modules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};
in {
  imports = [modules];

  # FIXME: what is the point of modules vs imports?
  modules = [
    nix-colors.homeManagerModule
    (_: {imports = [../lib/home];})
  ];

  importables = {inherit peers profiles roles;};

  users = {
    "cdom@prod-www.klein.temple.edu" = hmArgs: {
      imports = with hmArgs.roles;
        developer ++ remote;
      home.username = hmArgs.lib.mkForce "cdom";
      home.homeDirectory = hmArgs.lib.mkForce "/home/cdom";
    };

    "cdom@dev.klein.temple.edu" = hmArgs: {
      imports = with hmArgs.roles;
        developer ++ remote;
      home.username = hmArgs.lib.mkForce "cdom";
      home.homeDirectory = hmArgs.lib.mkForce "/home/cdom";
    };
  };
}
