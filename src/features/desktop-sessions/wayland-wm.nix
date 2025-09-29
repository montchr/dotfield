{
  aspects.desktop-sessions__wayland-wm = {
    requires = [
      # FIXME: not necessarily -- and this prevents usage alongside
      # gnome desktop session
      "login__regreet"

      # FIXME: gdm crashes to black screen without a gnome session
      # available.  there are numerous reports out there of this
      # happening, and i'm not quite sure what exactly gdm needs.
      # apparently it *is* possible to use gdm + sway.
      # "login__gdm"

      "secret-service__gnome-keyring"
    ];

    nixos =
      { flake, pkgs, ... }:
      {
        nix.settings.substituters = [ "https://nixpkgs-wayland.cachix.org" ];

        # Required for lockers to perform authentication.
        security.pam.services.swaylock = { };
        security.pam.services.waylock = { };

        xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];

        environment.sessionVariables = {
          # <https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayland>
          "ELECTRON_OZONE_PLATFORM_HINT" = "auto";
          "SDL_VIDEODRIVER" = "wayland";
          "QT_QPA_PLATFORM" = "wayland";
          "QT_SCALE_FACTOR_ROUNDING_POLICY" = "RoundPreferFloor";
          "QT_WAYLAND_DISABLE_WINDOWDECORATION" = "1";
          "_JAVA_AWT_WM_NONREPARENTING" = "1";
        };

        environment.systemPackages = with pkgs; [
          brightnessctl
          fuzzel
          grim
          gtk-layer-shell
          nautilus
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
          hyprpicker

          ## Screenshots/capture/annotation:
          grim # image grabber
          kooha # simple gui screen recorder
          satty # annotator
          slurp # capture region to stdout
          wf-recorder # wlr screen recorder

          ## Menus:
          wlogout

          ## Document viewers:
          pix # image manager
          swayimg
          zathura

          ## Wallpaper:
          swaybg
        ];
      };
  };

}
