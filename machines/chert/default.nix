{ pkgs, flake, ... }:
let
  inherit (flake.inputs) disko nixpkgs;
in
{
  imports = [
    disko.nixosModules.disko
    ./boot.nix
    ./mail/common.nix
    ./networking.nix

    # ./secrets
    # ./users
  ];

  disko.devices = import ./disk-config.nix { inherit (nixpkgs) lib; };

  system.stateVersion = "23.05";
}
