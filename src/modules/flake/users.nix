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
                      name = mkAspectNameOpt name;
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
                  aspects = mkAspectListOpt ''
                    List of baseline aspects shared by all of this user's configurations.

                    Note that the "core" aspect
                    (`users.<username>.aspects.core`) will *always* be
                    included in all of the user's configurations.  This
                    follows the same behavior as the "core" aspect in
                    the system scope, which is included in all system
                    configurations.
                  '';
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
