{ config, lib, options, pkgs, ... }:
let
  configDir = "${config.dotfield.configDir}/yabai";

  scripts = with pkgs; {
    # TODO: DRY this up
    border = (writeScriptBin "yabai-border"
      (builtins.readFile "${configDir}/bin/yabai-border"));

    closeWindow = (writeScriptBin "yabai-close-window"
      (builtins.readFile "${configDir}/bin/yabai-close-window"));

    focusDirection = (writeScriptBin "yabai-focus-direction"
      (builtins.readFile "${configDir}/bin/yabai-focus-direction"));

    kludge = (writeScriptBin "yabai-kludge"
      (builtins.readFile "${configDir}/bin/yabai-kludge"));

    setPadding = (writeScriptBin "yabai-set-padding"
      (builtins.readFile "${configDir}/bin/yabai-set-padding"));
  };
in {
  options = with lib; {
    my.modules.yabai = {
      enable = mkEnableOption ''
        Whether to enable yabai module
      '';
    };
  };

  config = {
    my.user = {
      packages = with builtins;
        (map (key: getAttr key scripts) (attrNames scripts));
    };

    launchd.user.agents.yabai.serviceConfig = {
      StandardOutPath = "${config.my.xdgPaths.cache}/yabai.out.log";
      StandardErrorPath = "${config.my.xdgPaths.cache}/yabai.err.log";
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
        ${scripts.setPadding}/bin/yabai-set-padding 12

        yabai -m space 1 --label 'task'
        yabai -m space 2 --label 'inspect'
        yabai -m space 3 --label 'code'
        yabai -m space 4 --label 'comm'
        yabai -m space 5 --label 'term'

        # Default to all Emacs windows unmanaged.
        # This will prevent childframes from becoming managed automatically,
        # which needs to happen quickly.
        yabai -m rule --add app='Emacs' \
          manage=off \
          mouse_follows_focus=off

        # Manage normal windows. Does not seem to affect childframes.
        yabai -m rule --add app='Emacs' \
          title=" ▲ doom(\s+(-|–|—){1}\s+\(\d+.+\d+\))?$" \
          manage=on

        # Float and center the doom capture window
        yabai -m rule --add app='Emacs' title="doom-capture" \
          manage=off \
          grid=3:3:1:1:1:1

        # Float Emacs childframes.
        # FIXME: doesn't work
        # yabai -m rule --add app='Emacs' \
        #   title='(Emacs\.app\s.\sdoom)\s{0,2}(-|–|—){1}\s+\(\d+.+\)$' \
        #   manage=off \
        #   mouse_follows_focus=off

        # Log info about each Emacs window for some more insight.
        # TODO: Remove once we know more.
        # yabai -m signal --add \
        #   event=window_focused \
        #   action='yabai -m query --windows --window' \
        #   app='Emacs'

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
