{
  config,
  inputs,
  pkgs,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [./nixpkgs-wayland.nix];
}
