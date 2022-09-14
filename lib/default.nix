{
  lib,
  peers,
  flake,
}: let
  inherit (builtins) mapAttrs;
  inherit (lib) makeExtensible attrValues foldr;
  inherit (flake.inputs.digga.lib) rakeLeaves;

  dotfieldLib = makeExtensible (self: (mapAttrs
    (name: path: (import path {inherit flake self lib peers;}))
    (rakeLeaves ./src)));
in
  # for convenience, provide all namespaced functions at the top level,
  # following the example of `nixpkgs.lib`.
  (dotfieldLib.extend
    (lfinal: lprev: (foldr
      (a: b: a // b) {} (attrValues lprev))))
