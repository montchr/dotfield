# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  aspects.desktop-sessions__wayland-wm = {
    requires = [
      # FIXME: not necessarily -- and this prevents usage alongside
      # gnome desktop session
      "login__noctalia-greeter"

      # FIXME: gdm crashes to black screen without a gnome session
      # available.  there are numerous reports out there of this
      # happening, and i'm not quite sure what exactly gdm needs.
      # apparently it *is* possible to use gdm + sway.
      # "login__gdm"
    ];

    nixos =
      { flake, pkgs, ... }:
      {
        # Required for lockers to perform authentication.
        security.pam.services.swaylock = { };
        security.pam.services.waylock = { };

        xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];

        environment.sessionVariables = {
          # <https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayland>
          "ELECTRON_OZONE_PLATFORM_HINT" = "auto";
          "SDL_VIDEODRIVER" = "wayland";
          "QT_QPA_PLATFORM" = "wayland-egl";
          "QT_SCALE_FACTOR_ROUNDING_POLICY" = "RoundPreferFloor";
          "QT_WAYLAND_FORCE_DPI" = "physical";
          "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
          "_JAVA_AWT_WM_NONREPARENTING" = "1";
        };

        environment.systemPackages = with pkgs; [
          brightnessctl
          fuzzel
          grim
          gtk-layer-shell
          satty
          slurp
          swaybg
          swayidle
          swayimg
          swaylock
          swaylock-effects
          wev
          wlogout
          wf-recorder
          wl-clipboard
          wlr-randr
          xwayland-satellite
        ];
      };

    home =
      { pkgs, ... }:
      {
        programs.swaylock.enable = true;

        home.packages = with pkgs; [
          hyprpicker
          wdisplays

          ## Screenshots/capture/annotation:
          grim
          satty # annotator
          slurp
          wayfarer
          wf-recorder

          ## Menus:
          wlogout

          ## Document viewers:
          pix # image manager
          zathura

          ## Wallpaper:
          swaybg
        ];
      };
  };

}
