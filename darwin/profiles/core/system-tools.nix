{
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake.inputs) nix-index-database;
in
{
  imports = [ nix-index-database.darwinModules.nix-index ];

  environment.systemPackages = [
    pkgs.m-cli
    pkgs.mas
    # pkgs.prefmanager
  ];
}
