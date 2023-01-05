{
  self,
  peers,
  ...
}: let
  inherit (self) inputs systems;
  inherit (inputs.digga.lib) rakeLeaves;
  l = inputs.nixpkgs.lib // builtins;
in {
  flake.lib = l.makeExtensible (lself: (l.mapAttrs
    (_: path: (import path {
      inherit l inputs peers systems;
      self = lself;
    }))
    (rakeLeaves ./src)));
}
