{ lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.dotfield.meta = {
    keys = mkOption {
      type = with types; attrsOf (attrsOf str);
      default = { };
      description = "Host metadata configuration including hardware specs, networking, and user keys";
    };
    networks = mkOption {
      type = with types; attrsOf raw;
      default = { };
      description = "Services metadata";
    };
    services = mkOption {
      type = with types; attrsOf raw;
      default = { };
      description = "Services metadata";
    };
  };

}
