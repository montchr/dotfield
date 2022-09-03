{
  inputs,
  lib,
  ...
}: let
  inherit (lib) mapAttrs;
  inherit (inputs.digga.lib) flattenTree;
in {
  importLeaves = leaves: mapAttrs (n: v: import v) (flattenTree leaves);
}
