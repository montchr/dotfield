{
  lib,
  flake-parts-lib,
  inputs,
  self,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (flake-parts-lib) mkPerSystemOption;

  mkName = name: "${name}-shell";
in
{
  options.perSystem = mkPerSystemOption (
    {
      config,
      options,
      pkgs,
      ...
    }:
    let
      moduleArgsModule = {
        _module.args = {
          inherit inputs self;
        };
      };
    in
    {
      options.umwelt = {
        modules = mkOption {
          type = types.listOf types.deferredModule;
          default = [ ];
        };
        shells = mkOption {
          type = types.attrsOf (
            types.submoduleWith { modules = [ moduleArgsModule ] ++ config.umwelt.modules; }
          );
          default = { };
        };
      };

      config = {
        devShells = lib.mapAttrs (
          name: devshell: devshell.finalPackage.overrideAttrs { name = mkName name; }
        );
        checks = lib.mapAttrs' (
          name: devshell: {
            name = mkName name;
            value = devshell.finalPackage;
          }
        );
      };
    }
  );
}
