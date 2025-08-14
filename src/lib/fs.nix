{ config, ... }:
let
  inherit (config.lib.fs) tree;
in
{
  flake.lib.fs = {
    importTree = tree;
  };
}
