moduleArgs @ {pkgs, ...}: {
  imports = [../common.nix];

  wayland.windowManager.sway = {
    enable = true;
    # A `null` value tells home-manager to use the package from the system level.
    package =
      if (moduleArgs.osConfig.programs.sway.enable or false)
      then null
      else pkgs.sway;
    systemdIntegration = true; # default
    config = {
      terminal = "kitty";
      modifier = "Mod4";
      # output = {};
      seat = {
        "*" = {
          hide_cursor = "when-typing enable";
        };
      };
    };
  };
}
