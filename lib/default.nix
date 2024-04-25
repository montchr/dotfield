{
  inputs,
  self,
  config,
  withSystem,
  ops,
  ...
}:
let
  haumea = inputs.haumea.lib;

  lib = haumea.load {
    src = ./src;
    inputs = {
      inherit ops withSystem;
      flake = {
        inherit self inputs config;
      };
    };
  };
in
{
  flake.lib = lib // {
    inherit ops;
  };
}
