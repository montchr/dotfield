{
  self,
  peers,
  ...
}: let
  inherit (self) inputs systems;
  l = inputs.nixpkgs.lib // builtins;

  lib = l.makeExtensible (lself: let
    callLibs = file:
      import file {
        inherit l inputs peers;
        lib = lself;
        supportedSystems = systems;
      };
  in {
    systems = callLibs ./systems.nix;
    peers = callLibs ./peers.nix;
  });
in {flake = {inherit lib;};}
