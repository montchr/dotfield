{lib, ...}: let
  inherit (lib) mapAttrs;
  inherit (lib.digga) flattenTree;
in {
  importLeaves = leaves: mapAttrs (_n: import) (flattenTree leaves);
}
