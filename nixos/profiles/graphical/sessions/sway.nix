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
    ./__wlroots.nix
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    xwayland.enable = true;
    extraOptions = optional hasNvidia "--unsupported-gpu";
    extraSessionCommands = ''
      # <https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayla[Espanso]: An error occurred during rendering, please examine the logs for more information.
      export ELECTRON_OZONE_PLATFORM_HINT="wayland"
      export SDL_VIDEODRIVER="wayland"
      export QT_QPA_PLATFORM="wayland-egl"
      export QT_SCALE_FACTOR_ROUNDING_POLICY="RoundPreferFloor"
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export _JAVA_AWT_WM_NONREPARENTING="1"
    '';
  };

  environment.etc."sway/config".text = ''
    exec sleep 5; systemctl --user start kanshi.service

    # <https://wiki.archlinux.org/title/Sway#Custom_keybindings>
    bindsym XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +5%
    bindsym XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -5%
    bindsym XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
    bindsym XF86AudioMicMute exec pactl set-source-mute @DEFAULT_SOURCE@ toggle
    bindsym XF86MonBrightnessDown exec brightnessctl set 5%-
    bindsym XF86MonBrightnessUp exec brightnessctl set 5%+
    # These should be set in user config:
    # bindsym XF86AudioPlay exec playerctl play-pause
    # bindsym XF86AudioNext exec playerctl next
    # bindsym XF86AudioPrev exec playerctl previous
    # bindsym XF86Search exec fuzzel
  '';

  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  xdg.portal.wlr.enable = true;

  programs.light.enable = true;
  # programs.obs-studio.plugins = with pkgs.obs-studio-plugins; [
  #   wlrobs
  # ];

  environment.systemPackages = with pkgs; [
    ##: core
    sway
    swayidle
    swaylock

    # TODO: profile
    #    wlr-which-key
  ];
}
