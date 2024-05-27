{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  isGnomeDesktop = config.services.xserver.desktopManager.gnome.enable;
in
{
  programs.dconf.enable = true;
  environment.systemPackages = [ pkgs.gnome.dconf-editor ];
  xdg.portal.extraPortals = lib.optional (!isGnomeDesktop) pkgs.xdg-desktop-portal-gtk;

  home-manager.sharedModules = [ "${flake.path}/home/profiles/desktop/gtk.nix" ];
}
