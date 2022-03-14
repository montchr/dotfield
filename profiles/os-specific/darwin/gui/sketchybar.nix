{ config, lib, pkgs, inputs, ... }:

let
  inherit (config) my;
  inherit (config.home-manager.users.${my.username}.lib.file) mkOutOfStoreSymlink;

  configDir = "${config.dotfield.path}/config/sketchybar";
in

{
  homebrew.brews = [ "sketchybar" ];

  services.yabai.config.external_bar = "main:32:0";

  # The system menu bar must be hidden when using a custom bar.
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  my.hm.xdg.configFile."sketchybar".source = mkOutOfStoreSymlink configDir;
}
