{ flake, lib, ... }:
let
  homeProfiles = import "${flake.self}/home/profiles.nix" { inherit (flake.inputs) haumea; };
in
{
  home-manager.sharedModules = lib.singleton homeProfiles.laptop;
}
