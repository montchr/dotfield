{ config, lib, pkgs, ... }:

let
  inherit (config.home-manager.users.${config.my.username}.lib.file) mkOutOfStoreSymlink;
in

{
  my.user.packages = with pkgs; [ ranger ];

  my.hm.xdg.configFile = {
    "ranger/rc.conf".source = mkOutOfStoreSymlink (toString ./rc.conf);
    "ranger/rifle.conf".source = mkOutOfStoreSymlink (toString ./rifle.conf);
  };
}
