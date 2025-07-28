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
  inherit (inputs) import-tree;
  haumea = inputs.haumea.lib;
in
{
  imports = [
    (import-tree ./lib)
    (import-tree ./features)
    (import-tree ./modules)

    ./packages
  ];
}
