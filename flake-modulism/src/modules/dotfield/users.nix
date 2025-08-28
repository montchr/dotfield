{
  moduleLocation,
  config,
  lib,
  self,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib'.modules) mkAspectListOpt mkAspectNameOpt mkDeferredModuleOpt;
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
                User-specific feature definitions.

                Note that due to these aspects' nature as user-specific, they
                may not define NixOS modules, which would affect the entire system.
              '';
            };
          };
        }
      )
    );
    default = { };
    description = "Baseline user aspects and home configurations";
  };
}
