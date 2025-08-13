{ lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.dotfield.meta.services = mkOption {
    type = with types; attrsOf raw;
    default = { };
    description = "Services metadata";
  };
}
