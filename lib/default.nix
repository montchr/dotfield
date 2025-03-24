{
  inputs,
  self,
  config,
  withSystem,
  ops,
  lib,
  ...
}:
let
  haumea = inputs.haumea.lib;

  lib' = haumea.load {
    src = ./src;
    inputs = {
      inherit lib ops withSystem;
      flake = {
        inherit self inputs config;
      };
    };
  };
in
{
  flake.lib = lib' // {
    inherit ops;
  };
}
