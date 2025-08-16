{
  lib,
  self,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib'.modules) mkDeferredModuleOpt mkFeatureListOpt;
  lib' = self.lib;
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
            baseline = mkOption {
              type = types.submodule {
                options = {
                  features = mkFeatureListOpt "List of baseline features shared by all of this user's configurations";
                  home = mkDeferredModuleOpt "Baseline home configuration shared by all of this user's configurations";
                };
                description = "Baseline features and configurations shared by all of this user's configurations";
              };
            };
          };
        }
      )
    );
    default = { };
    description = "Baseline user features and home configurations";
  };
}
