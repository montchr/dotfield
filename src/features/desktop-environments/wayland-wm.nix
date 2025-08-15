{
  dotfield.features.wayland-wm.nixos =
    { pkgs, ... }:
    {
      # UWSM a standardized entrypoint for Wayland compositors.
      # NOTE: Each supported compositor must be added to `programs.uwsm.waylandCompositors`.
      programs.uwsm.enable = true;

      # Required for lockers to perform authentication.
      security.pam.services.swaylock = { };
      security.pam.services.waylock = { };

      xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];

      environment.sessionVariables = {
        # <https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayland>
        "ELECTRON_OZONE_PLATFORM_HINT" = "wayland";
        "SDL_VIDEODRIVER" = "wayland";
        "QT_QPA_PLATFORM" = "wayland-egl";
        "QT_SCALE_FACTOR_ROUNDING_POLICY" = "RoundPreferFloor";
        "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
        "_JAVA_AWT_WM_NONREPARENTING" = "1";
      };

      # TODO: provide a default launcher
      environment.systemPackages = with pkgs; [
        gtk-layer-shell

        hyprpicker # color picker
        swayidle
        swayimg
        swaylock
        swaylock-effects
        wlogout
      ];
    };

  dotfield.features.wayland-wm.home =
    { config, ... }:
    {
      programs.eww.enable = true;
      home.packages = [
        config.programs.eww.package
      ];
    };
}
