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
in
{
  imports = [
    ./packages
  ];

  flake.lib = haumea.load {
    src = ./lib;
    inputs = {
      inherit lib ops withSystem;
      flake = {
        inherit self inputs config;
      };
    };
  };
}
