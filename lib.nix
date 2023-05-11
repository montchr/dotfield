{inputs, ...}: let
  inherit (inputs) haumea;
  lib = haumea.lib.load {
    src = ./lib/src;
    inputs = {inherit inputs;};
    transformer = haumea.lib.transformers.liftDefault;
  };
in {
  flake = {inherit lib;};
}
