{ lib, pkgs, ... }:
(lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) { programs.obs-studio.enable = true; })
