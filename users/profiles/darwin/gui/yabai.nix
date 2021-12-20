{ config, lib, pkgs, ... }:
let
  inherit (pkgs) writeScriptBin writeShellScriptBin;

  cfg = config.services.yabai;
  configDir = "${config.dotfield.configDir}/yabai";
  daemonPath = "/Library/LaunchDaemons/org.nixos.yabai-sa.plist";

  defaults = {
    padding = "6";
  };

  mkScriptFromFile = name: (writeScriptBin "yabai-${name}"
    (builtins.readFile "${configDir}/bin/${name}"));

  scriptsFromFiles = (map (n: mkScriptFromFile n) [
    "close-window"
    "focus-direction"
  ]);

  scripts =
    (builtins.listToAttrs (map
      (drv: {
        name = drv.name;
        value = drv;
      })
      scriptsFromFiles)) // {

      set-border =
        if cfg.border.enable
        then
          (mkScriptFromFile "set-border")
        else
          (writeShellScriptBin "yabai-set-border" "return 0");

      kickstart-sa = (writeShellScriptBin "yabai-sa-kickstart" ''
        # ${config.my.nix_managed}
        #
        # yabai-sa-kickstart
        #
        # Kickstart the scripting addition in case it fails to load.
        #

        set -x

        # See https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(from-HEAD)#updating-to-latest-head
        [[ $(sudo launchctl list | grep yabai-sa) ]] && {
          sudo launchctl unload ${daemonPath}
        }
        sudo yabai --uninstall-sa
        sudo yabai --install-sa
        sudo launchctl load ${daemonPath}

        set +x
      '');

      # Set padding and window gaps.
      set-padding = (writeShellScriptBin "yabai-set-padding" ''
        # ${config.my.nix_managed}
        #
        # yabai-set-padding
        #
        # Usage:
        #   yabai-set-padding [<padding-value>]
        #   yabai-set-padding 6
        #
        # Note that environment variables may not be available when the
        # config is loaded for the first time, so it's best to set the desired
        # values explicitly as an initial fallback -- a default for the default.
        #


        DEFAULT_PADDING="''${YABAI_PADDING_DEFAULT:-${defaults.padding}}"

        PADDING="''${1:-$DEFAULT_PADDING}"

        yabai -m config top_padding "$PADDING"
        yabai -m config bottom_padding "$PADDING"
        yabai -m config left_padding "$PADDING"
        yabai -m config right_padding "$PADDING"
        yabai -m config window_gap "$PADDING"
      '');

      kludge = (writeShellScriptBin "yabai-kludge" ''
        # ${config.my.nix_managed}
        #
        # yabai-kludge
        #
        # For when things... get real bad.
        #
        # A useful implement when you load an outdated version of the yabai scripting
        # addition and need to restore Dock.app to its original state.
        #
        # WARNING: This will erase any Dock settings, including icons, position, hide
        # status, etc.
        #
        # TODO: Logging.
        #
        # Source:
        #   https://forums.macrumors.com/threads/missing-dock-and-background-flashing-on-mavericks-gm.1650020/post-18213002

        # Remove potentially-corrupted files.
        ${toString (map (f: "rm $HOME/Library/Preferences/${f};") [
          "com.apple.spaces.plist"
          "com.apple.desktop.plist"
          "com.apple.dock.plist"
          "com.apple.dock.db"
        ])}

        # Restart the dock.
        killall Dock
      '');
    };

  # Get the store path to a yabai script by shortname.
  getScript = n: "${builtins.getAttr n scripts}/bin/yabai-${n}";

in
{

  my.user.packages = (map
    (key: builtins.getAttr key scripts)
    (builtins.attrNames scripts));

  environment.variables = {
    YABAI_BORDER_DARK = "000000";
    YABAI_BORDER_LIGHT = "ffffff";
    YABAI_BORDER_WIDTH = "4";
    YABAI_PADDING_DEFAULT = defaults.padding;
  };

  launchd.user.agents.yabai.serviceConfig = {
    StandardOutPath = "${config.my.xdg.cache}/yabai.out.log";
    StandardErrorPath = "${config.my.xdg.cache}/yabai.err.log";
  };

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = true;

    # As of 2021-12-19, border functionality is unstable in v4.0.0
    # https://github.com/koekeishiya/yabai/issues/1054#issue-1058384717
    border.enable = (! (pkgs.yabai.version == "4.0.0-pre"));

    config = {
      external_bar = "off";
      layout = "bsp";

      # FIXME: Default to `on` and write rules for disallowed applications
      # that this setting conflicts with.
      #
      # I tend to switch this on every so often because it seems like a good
      # idea. But I always end up disabling it because it causes lots of
      # frustrating issues when using GUIs with flyout menus activated on
      # hover (like iStat, for example).
      mouse_follows_focus = "off";

      # `autoraise` will override the effect of `window_topmost`
      # TODO: `autoraise` behavior was substatially improved in v4.0.0 -- check it out again
      focus_follows_mouse = "off";
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";

      # Default window layout
      window_placement = "second_child";
      # Display floating windows on top.
      window_topmost = "off";
      split_ratio = 0.5;
      auto_balance = "on";

      # Window opacity
      window_opacity = "on";
      window_shadow = "on";
      active_window_opacity = 1.0;
      normal_window_opacity = 0.95;

      # Enable window borders, but default to transparent.
      window_border = if cfg.border.enable then "on" else "off";
      normal_window_border_color = "0x00ffffff";
    };

    extraConfig =
      let
        signals = {
          logFocusedWindow = app: ''
            yabai -m signal --add \
              event=window_focused \
              action='yabai -m query --windows --window' \
              app='${app}'
          '';
        };

        rules = {
          unmanagedApps = (toString
            (map (app: ''yabai -m rule --add app="${app}" manage=off; '') [
              "1Password"
              "Affinity"
              "Alfred Preferences"
              "Fantastical Helper"
              "Harvest"
              "Stickies"
              "^System Preferences$"
            ]));
        };
      in
      ''
        # Set window padding to default value.
        ${getScript "set-padding"}

        yabai -m space 1 --label 'task'
        yabai -m space 2 --label 'inspect'
        yabai -m space 3 --label 'code'
        yabai -m space 4 --label 'comm'
        yabai -m space 5 --label 'term'

        # Float Emacs minibuffer
        # https://github.com/cmacrae/config/blob/303274bb5a97a6f1612d406d8d384482d3fa35f5/modules/macintosh.nix#L163
        yabai -m rule --add app='Emacs' \
          title='.*Minibuf.*' \
          manage=off \
          border=off

        # Float and center the doom capture window
        yabai -m rule --add app='Emacs' title="doom-capture" \
          manage=off \
          grid=3:3:1:1:1:1

        ${rules.unmanagedApps}

        yabai -m rule --add app="Microsoft Teams" opacity="1.0"
        yabai -m rule --add app="zoom.us" opacity="1.0"

      '';
  };
}
