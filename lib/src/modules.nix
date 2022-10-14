{lib, ...}: let
  inherit (lib) mapAttrs;
  inherit (lib.digga) flattenTree;
in {
  importLeaves = leaves: mapAttrs (n: v: import v) (flattenTree leaves);
}
