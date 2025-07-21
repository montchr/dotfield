{ config, ... }:
let
  inherit (config.flake.modules) nixos;
in
{
  flake.modules.nixos.user-cdom.imports = [
    nixos.admin-user

    ./modules/hardware/keyboard/kanata
  ];
}
