{ lib, pkgs, ... }:
{
  config = lib.mkIf pkgs.hostPlatform.isLinux { home.packages = [ pkgs.nyxt ]; };
}
