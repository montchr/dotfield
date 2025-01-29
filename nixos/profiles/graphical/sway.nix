{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) optional;
  inherit (config.dotfield.features) hasNvidia;
in
{
  imports = [
    ./wlroots/common.nix
    ./wlroots/kanshi.nix
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    xwayland.enable = true;
    extraOptions = optional hasNvidia "--unsupported-gpu";
    extraSessionCommands = ''
      # export SDL_VIDEODRIVER="wayland"
      # export QT_QPA_PLATFORM="wayland-egl"
      export QT_WAYLAND_FORCE_DPI="physical"
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
    '';
  };

  environment.etc."sway/config".text = ''
    exec sleep 5; systemctl --user start kanshi.service

    # Brightness
    bindsym XF86MonBrightnessDown exec light -U 10
    bindsym XF86MonBrightnessUp exec light -A 10

    # Volume
    bindsym XF86AudioRaiseVolume exec 'pactl set-sink-volume @DEFAULT_SINK@ +1%'
    bindsym XF86AudioLowerVolume exec 'pactl set-sink-volume @DEFAULT_SINK@ -1%'
    bindsym XF86AudioMute exec 'pactl set-sink-mute @DEFAULT_SINK@ toggle'
  '';

  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  xdg.portal.wlr.enable = true;

  programs.light.enable = true;

  environment.systemPackages = with pkgs; [
    ##: core
    sway
    swayidle
    # TODO
    # swaylock

    # dbus-sway-environment

    dunst
    fuzzel
    grim
    mako
    slurp
    wl-clipboard

    ##: gnome apps compatibility
    configure-gtk
    glib
    gnome3.adwaita-icon-theme
    gnome3.nautilus

    qt5.qtwayland
  ];
}
