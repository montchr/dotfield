{ flake, lib, ... }:
let
  homeProfiles = import "${flake.self}/home/profiles.nix" { inherit (flake.inputs) haumea; };
in
{
  # battery info
  services.upower.enable = true;

  home-manager.sharedModules = lib.singleton homeProfiles.laptop;
}
