{
  inputs,
  lib,
  peers,
}: let
  inherit (builtins) mapAttrs;
  inherit (lib) makeExtensible attrValues foldr;
  inherit (inputs.digga.lib) rakeLeaves;

  # FIXME: don't 'with self', it blurs scope
  dotfieldLib = makeExtensible (self: (with self;
    mapAttrs
    (name: path: (import path {inherit self lib inputs peers;}))
    (rakeLeaves ./src)));
in
  dotfieldLib.extend (lfinal: lprev:
    foldr (a: b: a // b) {} (attrValues lprev))
