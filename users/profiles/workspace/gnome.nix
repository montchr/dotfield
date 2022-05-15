moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  isGnomeDesktop = moduleArgs.nixosConfig.services.xserver.desktopManager.gnome.enable or false;
in {
  home.packages = with pkgs; (lib.optionals isGnomeDesktop [
    dconf2nix
    gnome.dconf-editor
    gnome.polari
    gnome.gnome-disk-utility
    gnome.gnome-tweak-tool

    # Extras:
    # gnomeExtensions.pop-os-shell
    gnomeExtensions.gsconnect
    gnomeExtensions.user-themes
  ]);
}
