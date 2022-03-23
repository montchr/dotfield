{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs) writeScriptBin writeShellScriptBin;
  inherit (inputs.gitignore.lib) gitignoreSource;

  cfg = config.services.yabai;
  # barCfg = config.services.sketchybar;

  configDir = "${config.dotfield.configDir}/yabai";
  daemonPath = "/Library/LaunchDaemons/org.nixos.yabai-sa.plist";

  defaults = {
    padding = "6";
  };

  mkArgString = lib.generators.toKeyValue {
    mkKeyValue = key: value: let
      value' =
        if lib.isBool value
        then
          (
            if value
            then "on"
            else "off"
          )
        else builtins.toString value;
    in "${key}='${value'}' \\";
  };

  mkRule = {app, ...} @ args: let
    args' =
      lib.filterAttrs
      (n: _: ! builtins.elem n ["app"])
      args;
  in ''
    yabai -m rule --add app='${app}' ${mkArgString args'}
  '';

  mkSignal = {
    event,
    action,
    ...
  } @ args: let
    args' =
      lib.filterAttrs
      (n: _: ! builtins.elem n ["event" "action"])
      args;
  in ''
    yabai -m signal --add \
      event='${event}' \
      action='${action}' \
      ${mkArgString args'}
  '';

  mkRules = rules: lib.strings.concatMapStringsSep "\n" (x: mkRule x) rules;
  mkSignals = signals: lib.strings.concatMapStringsSep "\n" (x: mkSignal x) signals;

  mkScriptFromFile = name: (writeScriptBin "yabai-${name}"
    (gitignoreSource "${configDir}/bin/${name}"));

  scriptsFromFiles = map (n: mkScriptFromFile n) [
    "close-window"
    "focus-direction"
  ];

  scripts =
    (builtins.listToAttrs (map
      (drv: {
        name = drv.name;
        value = drv;
      })
      scriptsFromFiles))
    // {
      kickstart-sa = writeShellScriptBin "yabai-sa-kickstart" ''
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
      '';

      # Set padding and window gaps.
      set-padding = writeShellScriptBin "yabai-set-padding" ''
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
      '';

      kludge = writeShellScriptBin "yabai-kludge" ''
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
      '';
    };

  # Get the store path to a yabai script by shortname.
  getScript = n: "${builtins.getAttr n scripts}/bin/yabai-${n}";
in {
  my.user.packages =
    map
    (key: builtins.getAttr key scripts)
    (builtins.attrNames scripts);

  environment.variables = {
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

    config = {
      external_bar = false;

      # TODO: set based on external bar config
      # if barCfg.enable
      # then
      #   let
      #     position = barCfg.config.position;
      #     height = barCfg.config.height;
      #   in
      #   (lib.concatStringsSep ":" [
      #     (barCfg.config.display)
      #     (if "top" == position then (toString height) else "0")
      #     (if "bottom" == position then (toString height) else "0")
      #   ])
      # else "off";

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
      # Disabled due to possible issues with external displays
      window_opacity = "off";
      window_shadow = "off";
    };

    extraConfig = let
      commonRules = {
        manage = false;
        sticky = true;
      };

      rules = mkRules [
        # FIXME: these are not working!
        (commonRules // {app = "1Password";})
        (commonRules // {app = "Alfred Preferences";})
        (commonRules // {app = "Fanatastical Helper";})
        (commonRules // {app = "Harvest";})
        (commonRules // {app = "Stickies";})
        (commonRules // {app = "^System Preferences$";})

        {
          app = "Affinity";
          manage = false;
        }
        {
          app = "Microsoft Teams";
          opacity = "1.0";
        }
        {
          app = "zoom.us";
          opacity = "1.0";
        }

        ## Emacs

        {
          app = "Emacs";
          title = "doom-capture";
          manage = false;
          grid = "3:3:1:1:1:1";
          label = "[Emacs]: Float and center the doom capture window";
        }
        {
          app = "Emacs";
          title = ".*Minibuf.*";
          manage = false;
          label = "[Emacs]: Float minibuffer";
        }
      ];

      signals = mkSignals [
        {
          event = "window_focused";
          action = "yabai -m query --windows --window";
          label = "log each focused window";
        }
      ];
    in ''
      # Set window padding to default value.
      ${getScript "set-padding"}

      ###: NOTEBOOK WORKSPACES {{{

        #: 'connect' :: slack, zoom, any others related to a current meeting
        yabai -m space 1 --label 'connect'

        #: 'browse' :: firefox for the machine's primary profile
        yabai -m space 2 --label 'browse'

        #: 'test' :: additional browser, other utils for testing current task
        yabai -m space 3 --label 'test'

        #: 'code' :: emacs, sometimes kitty
        yabai -m space 4 --label 'code'

        #: 'term' :: kitty
        yabai -m space 5 --label 'term'

        #: 'media' :: audio/video player, e.g. plexamp, spotify, youtube, vlc
        yabai -m space 6 --label 'media'

        #: 'browse-alt' :: additional firefox instance for secondary profile
        yabai -m space 7 --label 'browse-alt'

      ### }}}


      ###: DESKTOP WORKSPACES {{{

        ##: CENTER DISPLAY {{

          yabai -m space 1 --label 'connect'
          yabai -m space 2 --label 'browse'
          yabai -m space 3 --label 'code'

        ## }}

        ##: RIGHT DISPLAY {{

          yabai -m space 1 --label 'term'

          #: 'chat' :: slack, irc, signal, etc.
          yabai -m space 2 --label 'chat'

          yabai -m space 3 --label 'media'
          yabai -m space 4 --label 'browse-alt'

        ## }}

      ### }}}

      ${rules}
      ${signals}
    '';
  };
}
