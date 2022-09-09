{
  lib,
  peers,
}: let
  inherit (lib) makeExtensible attrValues foldr;
  inherit (lib.exo.digga) rakeLeaves;

  dotfieldLib = makeExtensible (self:
    builtins.mapAttrs
    (name: path: (import path {inherit self lib peers;}))
    (rakeLeaves ./src));
in
  dotfieldLib.extend (lfinal: lprev:
    foldr (a: b: a // b) {} (attrValues lprev))
