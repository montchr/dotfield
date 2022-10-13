{
  l,
  inputs,
  ...
}: let
  inherit (inputs.digga.lib) flattenTree;
in {
  importLeaves = leaves: l.mapAttrs (n: v: import v) (flattenTree leaves);
}
