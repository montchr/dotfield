moduleArgs @ {
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) add;
  inherit (inputs.nix-std.lib.num) toFloat;
  inherit (config) theme;
in {
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
      # FIXME: don't assume terminal
      terminal = "kitty";
      fonts = {
        # NOTE: the order is important: icon font must go before normal text font.
        names = [
          theme.fonts.symbols.family
          theme.fonts.mono.family
        ];
        # Window titles should be slightly larger than the normal text size.
        size = toFloat (add theme.fonts.mono.size 2);
        # style = "Bold";
      };
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
