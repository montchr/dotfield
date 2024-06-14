# Tracking issue: <https://github.com/NixOS/nixpkgs/issues/107495>
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
in
{
  environment.systemPackages = lib.optionals (!isAarch64) [ pkgs.zoom-us ]; # <- broken
}
