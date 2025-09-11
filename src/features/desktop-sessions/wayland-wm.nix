{
  aspects.desktop-sessions__wayland-wm = {
    requires = [
      # FIXME: not necessarily -- and this prevents usage alongside
      # gnome desktop session
      "login__regreet"
      "secret-service__gnome-keyring"
    ];

    nixos =
      { flake, pkgs, ... }:
      {
        nix.settings.substituters = [ "https://nixpkgs-wayland.cachix.org" ];

        # UWSM provides a standardized entrypoint for Wayland compositors.
        # NOTE: Each supported compositor must be added to `programs.uwsm.waylandCompositors`.
        programs.uwsm.enable = true;

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
          "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
          "_JAVA_AWT_WM_NONREPARENTING" = "1";
        };

        # TODO: provide a default launcher
        environment.systemPackages = with pkgs; [
          gtk-layer-shell

          # essentials
          brightnessctl
          fuzzel
          grim
          satty
          slurp
          swaybg
          swayidle
          swaylock
          swaylock-effects
          wev
          wf-recorder
          wl-clipboard
          xwayland-satellite

          # swappables
          kanshi
          nemo # file manager
        ];
      };

    home =
      { pkgs, ... }:
      {
        # A secret service is required.  It's either pass-secret-service or
        # GNOME Keyring, but only one can be enabled at a time (they provide
        # mutually-exclusive implementations of the XDG Secret Service
        # protocol).  It's probably not the best idea to make
        # pass-secret-service a default requirement, since it requires the
        # user to have configured the password-store properly.  On top of
        # that, GNOME Keyring generally works more seamlessly and has more
        # integrations.  pass-secret-service will often spam the user for
        # their GPG or hardware key passphrase on session start (when a
        # startup service wants to use the secret service).
        #
        # Well, GNOME Keyring should be activated the system level, so this
        # is pointless.
        #
        # ../gnome-keyring.nix

        programs.swaylock.enable = true;

        home.packages = with pkgs; [
          ## Color picker:
          hyprpicker

          ## File manager:
          superfile

          ## Screenshots/capture/annotation:
          grim
          kooha
          satty
          slurp
          wf-recorder

          ## Menus:
          wlogout

          ## Document viewers:
          kdePackages.koko
          pix
          pqiv # or imv
          swayimg
          zathura

          ## Wallpaper:
          swaybg
          waypaper
        ];
      };
  };

}
