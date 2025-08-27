{
  self,
  inputs,
  config,
  withSystem,
  ...
}:
let
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
in
{
  flake.lib.modules = {
    inherit
      flakeSpecialArgs
      flakeSpecialArgs'
      ;
  };
}
