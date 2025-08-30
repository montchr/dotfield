{
  lib,
  self,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (self.lib.modules) mkAspectListOpt mkAspectNameOpt mkDeferredModuleOpt;
in
{
  options.users = mkOption {
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
                  configuration = mkDeferredModuleOpt "Baseline home configuration for the user";
                };
              };
              description = "Baseline configurations shared by all of this user's host-specific configurations";
              default = { };
            };
          };
        }
      )
    );
    default = { };
    description = "Attributes of Dotfield user specifications";
  };
}
