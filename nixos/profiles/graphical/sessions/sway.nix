{
  flake,
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

  programs.light.enable = true;

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
  '';

  environment.systemPackages = with pkgs; [
    ##: core
    sway
    swayidle
    swaylock

    # TODO: profile
    #    wlr-which-key
  ];
}
