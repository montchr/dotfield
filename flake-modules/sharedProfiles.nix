{
  lib,
  flake-parts-lib,
  ...
}: let
  inherit
    (lib)
    mkOption
    types
    ;
  inherit
    (flake-parts-lib)
    mkSubmoduleOptions
    ;
in {
  options = {
    flake = mkSubmoduleOptions {
      sharedProfiles = mkOption {
        type = types.lazyAttrsOf types.unspecified;
        default = {};
        description = ''
          Importable cross-OS configuration-only modules.
        '';
      };
    };
  };
}
