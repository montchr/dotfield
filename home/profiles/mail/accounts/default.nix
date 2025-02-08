{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

in
{
  imports = [
    ./protonmail.nix
  ];
}
