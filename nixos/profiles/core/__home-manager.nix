{ flake, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  inherit (flake.self.lib.hm) settings';
in
{
  home-manager = settings' system;
}
