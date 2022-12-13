{inputs, ...}: let
  inherit (inputs.digga.lib) flattenTree;
  l = inputs.nixpkgs.lib // builtins;
in {
  importLeaves = leaves: l.mapAttrs (_n: import) (flattenTree leaves);
}
