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
            aspects = mkOption {
              type = types.lazyAttrsOf (
                types.submodule (
                  { name, ... }:
                  {
                    options = {
                      _name = mkAspectNameOpt name;
                      home = mkDeferredModuleOpt "A Home-Manager module";
                    };
                  }
                )
              );
              default = { };
              description = ''
                User-specific aspect definitions.

                Note that due to these aspects' nature as user-specific, they
                may not define NixOS modules, which would affect the entire system.
              '';
            };
            baseline = mkOption {
              type = types.submodule {
                options = {
                  aspects = mkAspectListOpt "List of baseline aspects shared by all of this user's configurations";
                  configuration = mkDeferredModuleOpt "Baseline home configuration shared by all of this user's configurations";
                };
              };
              description = "Baseline aspects and configurations shared by all of this user's configurations";
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
