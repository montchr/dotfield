{ config, lib, options, pkgs, ... }:
let
  configDir = "${config.dotfield.configDir}";
in
{
  options = with lib; {
    my.modules.yabai = {
      enable = mkEnableOption ''
        Whether to enable yabai module
      '';
    };
  };

  config = {
    my.hm.file = {
      ".local/bin" = {
        recursive = true;
        source = "${configDir}/yabai/bin";
      };
    };

    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      enableScriptingAddition = true;

      config = {

        external_bar = "off";
        layout = "bsp";

        # Mouse behavior
        mouse_follows_focus = "off";

        # `autoraise` will override the effect of `window_topmost`
        focus_follows_mouse = "off";
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";

        # Default window layout
        window_placement = "second_child";
        # Display floating windows on top.
        window_topmost = "off";
        split_ratio = 0.5;
        auto_balance = "off";

        # Window opacity
        window_opacity = "off";
        window_shadow = "on";
        active_window_opacity = 1.0;
        normal_window_opacity = 0.9;
        # normal_window_opacity = 1.0;

        # Window borders
        window_border = "on";
        normal_window_border_color = "0x00505050";

      };

      extraConfig = ''
        $XDG_BIN_HOME/yabai-set-padding 12

        yabai -m space 1 --label 'task'
        yabai -m space 2 --label 'inspect' && yabai -m space 2
        yabai -m space 3 --label 'code' && yabai -m space 3
        yabai -m space 4 --label 'comm' && yabai -m space 4
        yabai -m space 5 --label 'term' && yabai -m space 5

        # Default to all Emacs windows unmanaged.
        yabai -m rule --add app='Emacs' \
          manage=off \
          mouse_follows_focus=off

        # Manage normal windows.
        yabai -m rule --add app='Emacs' \
          title=" ▲ doom(\s+(-|–|—){1}\s+\(\d+.+\d+\))?$" \
          manage=on

        # Float and center the doom capture window
        yabai -m rule --add app='Emacs' title="doom-capture" \
          manage=off \
          grid=3:3:1:1:1:1

        # Float Emacs childframes.
        # yabai -m rule --add app='Emacs' title='^\s{0,2}(-|–|—){1}\s+\(\d+.+\)$' \
        #   manage=off \
        #   mouse_follows_focus=off


        yabai -m rule --add app=1Password manage=off
        yabai -m rule --add app="Affinity" manage=off
        yabai -m rule --add app="Fantastical Helper" manage=off
        yabai -m rule --add app=Stickies manage=off
        yabai -m rule --add app='^System Preferences$' manage=off

        yabai -m rule --add app=Browserosaurus manage=off sticky=on
        yabai -m rule --add app=Harvest \
          manage=off

        # Float unnamed windows.
        #
        # @TODO enabling this, for some reason, prevents phpstorm windows from being
        # managed by yabai
        #
        # yabai -m rule --add title="^$" manage=off

        # caused yabai to crash?
        # yabai -m rule --add app=PhpStorm title='^$' manage=off
      '';
    };
  };

}
