collective @ {peers, ...}: {self, ...}: let
  inherit (self.inputs) digga nix-colors;
  inherit (digga.lib) importExportableModules rakeLeaves;

  homeModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles {inherit profiles;};

  defaultProfiles = with profiles; [
    core
    direnv
    navi
    nnn
    ranger
    secrets.common
    tealdeer
    vim
  ];
in {
  imports = [homeModules];
  modules =
    defaultProfiles
    ++ [
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
