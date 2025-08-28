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
          };
        }
      )
    );
    default = { };
    description = "Attributes of Dotfield user specifications";
  };
}
