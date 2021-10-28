{ config, lib, pkgs, ... }:
{
  imports = [
    ./primary
    ./modules/shell.nix
  ];
}
