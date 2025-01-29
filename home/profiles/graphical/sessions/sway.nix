moduleArgs@{ pkgs, ... }:
{
  imports = [ ../common.nix ];

  wayland.windowManager.sway = {
    enable = true;
    # A `null` value tells home-manager to use the package from the
    # system level.
    package = if (moduleArgs.osConfig.programs.sway.enable or false) then null else pkgs.sway;
    systemdIntegration = true; # default
    config = {
      modifier = "Mod4";
      terminal = "kitty";
      startup = [
        { command = "firefox --profile home"; }
        { command = "emacs"; }
      ];
      # output = {};
      # seat = {
      #   "*" = {
      #     hide_cursor = "when-typing enable";
      #   };
      # };
      floating.criteria = [
        { class = "Pavucontrol"; }
      ];
    };
  };
}
