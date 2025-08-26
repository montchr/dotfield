{ self, lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.dotfield = {
    options = mkOption {
      type = types.lazyAttrsOf types.raw;
      default = { };
    };
  };
}
