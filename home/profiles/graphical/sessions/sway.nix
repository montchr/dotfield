moduleArgs@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.wayland.windowManager.sway;
  fxCfg = config.programs.firefox;
  mod = cfg.config.modifier;
in
{
  imports = [
    ../common.nix
    ../darkman.nix
  ];

  wayland.windowManager.sway = {
    enable = true;
    # A `null` value tells home-manager to use the package from the
    # system level.
    package = if (moduleArgs.osConfig.programs.sway.enable or false) then null else pkgs.sway;
    config = {
      modifier = "Mod4";
      terminal = "ghostty";
      startup = [
        {
          command = "firefox --profile ~/.mozilla/firefox/home";
        }
        {
          command = "emacs";
        }
      ];
      # NOTE: lib.mkOptionDefault is required in order to not wipe out
      # default keybindings!  See the option description.
      keybindings = lib.mkOptionDefault {

        # "$mod+Shift+q" = "kill"; # default

        # NOTE: Most media keys are set in system config.  The
        # behavior of the following media keys is a matter of user
        # preference:
        "XF86AudioPlay" = "exec playerctl play-pause";
        "XF86AudioNext" = "exec playerctl next";
        "XF86AudioPrev" = "exec playerctl previous";
        "XF86Search" = "exec fuzzel";
      };
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
