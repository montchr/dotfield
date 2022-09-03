{
  config,
  lib,
  flake-parts-lib,
  self,
  ...
}: let
  inherit
    (lib)
    genAttrs
    mapAttrs
    mkOption
    types
    ;
  inherit
    (flake-parts-lib)
    mkPerSystemType
    ;

  darwinSystems = builtins.filter (s: lib.hasSuffix "darwin" s) config.systems;

  rootConfig = config;
in {
  options = {
    perDarwinSystem = mkOption {
      description = "A function from Darwin system to flake-like attributes omitting the <literal>&lt;system></literal> attribute.";
      type = mkPerSystemType ({
        config,
        system,
        ...
      }: {
        _file = ./perSystem.nix;
        config = {
          _module.args.inputs' = mapAttrs (k: rootConfig.perInput system) self.inputs;
          _module.args.self' = rootConfig.perInput system self;
        };
      });
      apply = modules: system:
        (lib.evalModules {
          inherit modules;
          prefix = ["perDarwinSystem" system];
          specialArgs = {
            inherit system;
          };
        })
        .config;
    };
  };

  config = {
    allSystems = genAttrs darwinSystems config.perDarwinSystem;
  };
}
