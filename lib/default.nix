{
  inputs,
  peers,
  systems,
}: let
  inherit (inputs.digga.lib) rakeLeaves;
  l = inputs.nixpkgs.lib // builtins;
in
  l.makeExtensible (self: (l.mapAttrs
    (_: path: (import path {inherit inputs self peers systems;}))
    (rakeLeaves ./src)))
