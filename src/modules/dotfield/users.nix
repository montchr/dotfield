{
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib'.modules) mkDeferredModuleOpt mkFeatureListOpt;
  lib' = config.lib;
in
{
  options.dotfield.users = mkOption {
    type = types.lazyAttrsOf (
      types.submodule (
        { name, ... }:
        {
          options = {
            name = mkOption {
              default = name;
              readOnly = true;
              description = "Username";
            };
            modules = mkFeatureListOpt "List of baseline features shared by all of this user's configurations";
            home = mkDeferredModuleOpt "Baseline home configuration shared by all of this user's configurations";
          };
        }
      )
    );
  };
}
