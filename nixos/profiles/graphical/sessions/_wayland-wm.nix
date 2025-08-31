{ flake, pkgs, ... }:
{
  imports = [
    ../nixpkgs-wayland-overlay.nix
    ../common.nix
    ../login/regreet.nix
    ../gnome-keyring.nix
  ];

  # UWSM provides a standardized entrypoint for Wayland compositors.
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
}
