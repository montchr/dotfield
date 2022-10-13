{
  self,
  peers,
  ...
}: let
  inherit (self) inputs;
  inherit (self.inputs.digga.lib) rakeLeaves;
  l = inputs.nixlib.lib // builtins;
  lib = l.makeExtensible (lself: (l.mapAttrs
    (name: path: (import path {
      inherit inputs l peers;
      self = lself;
    }))
    (rakeLeaves ./src)));
in {
  # For convenience, provide all namespaced functions at the top level.
  flake.lib =
    lib.extend (lfinal: lprev: (l.foldr
      (a: b: a // b) {} (l.attrValues lprev)));
}
