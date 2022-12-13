{
  inputs,
  peers,
}: let
  inherit (inputs.digga.lib) rakeLeaves;
  l = inputs.nixpkgs.lib // builtins;

  dotfieldLib = l.makeExtensible (self: (l.mapAttrs
    (_name: path: (import path {inherit inputs self peers;}))
    (rakeLeaves ./src)));
in
  # for convenience, provide all namespaced functions at the top level
  (dotfieldLib.extend
    (_lfinal: lprev: (l.foldr
      (a: b: a // b) {} (l.attrValues lprev))))
