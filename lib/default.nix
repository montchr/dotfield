{
  lib,
  peers,
}: let
  inherit (lib) makeExtensible attrValues foldr;
  inherit (lib.digga) rakeLeaves;

  dotfieldLib = makeExtensible (self:
    builtins.mapAttrs
    (name: path: (import path {inherit self lib peers;}))
    (rakeLeaves ./src));
in
  # for convenience, provide all namespaced functions at the top level,
  # following the example of `nixpkgs.lib`.
  dotfieldLib.extend (lfinal: lprev: foldr (a: b: a // b) {} (attrValues lprev))
