{
  lib,
  peers,
  inputs,
}: let
  inherit (builtins) mapAttrs;
  inherit (lib) makeExtensible attrValues foldr;
  inherit (inputs.digga.lib) rakeLeaves;

  dotfieldLib = makeExtensible (self: (mapAttrs
    (_name: path: (import path {inherit inputs self lib peers;}))
    (rakeLeaves ./src)));
in
  # for convenience, provide all namespaced functions at the top level,
  # following the example of `nixpkgs.lib`.
  (dotfieldLib.extend
    (_lfinal: lprev: (foldr
      (a: b: a // b) {} (attrValues lprev))))
