{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in
lib.mkMerge [
  (lib.mkIf isLinux {
    xsession.enable = true;
  })
]
