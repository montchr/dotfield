{ config, lib, pkgs, ... }:

lib.mkMerge [
  {
    home.packages = [pkgs._1password];
  }
  (lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    home.packages = [pkgs._1password-gui];
  })
]
