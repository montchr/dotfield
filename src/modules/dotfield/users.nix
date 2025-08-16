{
  moduleLocation,
  config,
  lib,
  self,
  ...
}:
let
  inherit (builtins) mapAttrs removeAttrs;
  inherit (lib) mkOption types;
  inherit (lib.strings) escapeNixIdentifier;
  inherit (lib'.modules) mkDeferredModuleOpt mkFeatureListOpt;
  lib' = self.lib;

  # https://github.com/hercules-ci/flake-parts/blob/af66ad14b28a127c5c0f3bbb298218fc63528a18/extras/modules.nix#L12-L22
  addInfo =
    username: class: moduleName:
    if class == "generic" then
      module: module
    else
      module:
      # TODO: set key?
      {
        _class = class;
        _file = "${toString moduleLocation}#dotfield.users.${escapeNixIdentifier username}.${escapeNixIdentifier moduleName}.${escapeNixIdentifier class}";
        imports = [ module ];
      };
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
              };
              description = "Baseline features and configurations shared by all of this user's configurations";
              default = { };
            };
            features = mkOption {
              type = types.lazyAttrsOf (
                types.submodule {
                  options = {
                    home = mkDeferredModuleOpt "A Home-Manager module";
                  };
                }
              );
              default = { };
              description = ''
                User-specific feature definitions.

                Note that due to these features' nature as user-specific, they
                may not define NixOS modules, which would affect the entire system.
              '';
            };
          };
        }
      )
    );
    default = { };
    description = "Baseline user features and home configurations";
  };
}
