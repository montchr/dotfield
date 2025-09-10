{ self, lib, ... }:
let
  inherit (lib) mkOption types;
  inherit (self.lib.modules) aspectSubmoduleGenericOptions mkAspectNameOpt;
  aspectSubmodule =
    { name, ... }:
    {
      options = aspectSubmoduleGenericOptions // {
        name = mkAspectNameOpt name;
      };
    };
in
{
  options.aspects = mkOption {
    type = types.lazyAttrsOf (types.submodule aspectSubmodule);
    default = { };
  };
}
