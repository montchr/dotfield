{config, ...}: {
  imports = [
    ./guardian.nix
  ];
  config = {
    home-manager.sharedModules = [
      ({config, ...}: {
        home.sessionVariables."DOTFIELD_DIR" = config.lib.dotfield.fsPath;
      })
    ];
  };
}
