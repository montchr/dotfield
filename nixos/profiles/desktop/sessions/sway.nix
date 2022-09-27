{
  config,
  lib,
  pkgs,
  nixosProfiles,
  ...
}: let
  inherit (lib) optional;
  inherit (dotfieldLib.sys) hasNvidia;
  dotfieldLib = config.lib.dotfield;
in {
  imports = with nixosProfiles; [
    desktop.common
  ];
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraOptions = optional hasNvidia "--unsupported-gpu";
    extraSessionCommands = ''
      export SDL_VIDEODRIVER="wayland"
      export QT_QPA_PLATFORM="wayland-egl"
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
    '';
  };
  xdg.portal.extraPortals = with pkgs; [xdg-desktop-portal-gtk];
  xdg.portal.wlr.enable = true;
  environment.systemPackages = with pkgs; [
    ##: core
    sway
    swayidle
    # TODO
    # swaylock

    dbus-sway-environment
    grim
    mako
    slurp
    waybar
    # wayland
    wl-clipboard

    ##: gnome apps compatibility
    configure-gtk
    glib
    gnome3.adwaita-icon-theme
    gnome3.nautilus

    qt5.qtwayland
  ];
}
