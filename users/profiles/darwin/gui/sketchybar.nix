{ config, lib, pkgs, inputs, ... }:

let
  inherit (config) my;
  inherit (config.home-manager.users.${my.username}.lib.file) mkOutOfStoreSymlink;

  configDir = "${config.dotfield.path}/config/sketchybar";
in

{
  homebrew.brews = [ "sketchybar" ];

  my.hm.xdg.configFile."sketchybar".source = mkOutOfStoreSymlink configDir;
}
