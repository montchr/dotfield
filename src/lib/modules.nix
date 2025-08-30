{
  lib,
  self,
  inputs,
  config,
  withSystem,
  ...
}:
let
  inherit (lib) mkOption types;

  flakeSpecialArgs = {
    inherit self inputs config;
  };

  flakeSpecialArgs' =
    system:
    withSystem system (
      ctx@{ config, inputs', ... }:
      let
        perSystem = {
          inherit (ctx.config) legacyPackages packages;
          inherit inputs';
        };
      in
      flakeSpecialArgs // { inherit perSystem; }
    );

  mkDeferredModuleOpt =
    description:
    mkOption {
      inherit description;
      type = types.deferredModule;
      default = { };
    };

in
{
  flake.lib.modules = {
    inherit
      flakeSpecialArgs
      flakeSpecialArgs'
      mkDeferredModuleOpt
      ;
  };
}
