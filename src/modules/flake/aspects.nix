{ lib, ... }:
let
  inherit (lib) mkOption types;
  aspectSubmodule =
    { name, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          default = name;
          readOnly = true;
          internal = true;
        };
        requires = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };
        features = mkOption {
          type = types.listOf types.path;
          default = [ ];
        };
        nixos = mkOption {
          type = types.deferredModule;
          default = { };
        };
        home = mkOption {
          type = types.deferredModule;
          default = { };
        };
      };
    };
in
{
  options.aspects = mkOption {
    type = types.lazyAttrsOf (types.submodule aspectSubmodule);
    default = { };
  };
}
