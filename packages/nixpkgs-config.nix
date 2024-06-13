{ overlays, allowUnfree }:
{ lib, config, ... }:
let
  inherit (builtins) elem isBool isList;
  inherit (lib) getName mkOption types;
in
{
  options.dotfield.nixpkgs = {
    allowedUnfreePackages = mkOption {
      default = [ ];
      type = with types; listOf str;
      description = ''
        Default allowed unfree packages.
      '';
    };
  };

  config = {
    nixpkgs = {
      inherit overlays;
      config.allowUnfree = if (isBool allowUnfree) then allowUnfree else false;
      config.allowUnfreePredicate =
        if (isList allowUnfree) then
          (pkg: elem (getName pkg) (allowUnfree ++ config.dotfield.nixpkgs.allowedUnfreePackages))
        else
          (_: true);
    };
  };
}
