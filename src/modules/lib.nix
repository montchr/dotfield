{ lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.flake.lib = mkOption {
    description = "Internal helpers library";
    type = with types; lazyAttrsOf raw;
    default = { };
  };
}
