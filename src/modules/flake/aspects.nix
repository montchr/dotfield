{ self, lib, ... }:
let
  inherit (lib) mkOption types;
  inherit (self.lib.modules) aspectSubmoduleGenericOptions;
  aspectSubmodule =
    { name, ... }:
    {
      options = aspectSubmoduleGenericOptions // {
        name = mkOption {
          type = types.str;
          default = name;
          readOnly = true;
          internal = true;
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
