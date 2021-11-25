{ config, lib, pkgs, ... }:

{
  imports = [
    ./karabiner.nix
    ./skhd.nix
  ];
}
