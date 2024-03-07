{ pkgs, flake, ... }:
let
  inherit (flake.inputs) disko nixpkgs;
in
{
  imports = [
    disko.nixosModules.disko
    ./boot.nix
    ./networking.nix
    ./mailserver/default.nix
    ./mailserver/roundcube.nix
    ./secrets
    ./users
  ];

  disko.devices = import ./disk-config.nix { inherit (nixpkgs) lib; };

  # Include common mail utilities for testing, since this is a mailserver.
  environment.systemPackages = [
    pkgs.aerc
    pkgs.isync
    pkgs.mu
    pkgs.mutt
  ];

  system.stateVersion = "23.05";
}
