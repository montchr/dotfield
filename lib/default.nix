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
      # inherit (inputs.nixpkgs) lib;
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
