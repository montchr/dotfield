{
  config,
  lib,
  pkgs,
  self,
  system,
  ...
}: let
  inherit (pkgs) writeScriptBin writeShellScriptBin;

  cfg = config.services.yabai;
  # barCfg = config.services.sketchybar;

  yabaiPackage = self.packages.${system}.yabai;

  configDir = "${pkgs.dotfield-config}/yabai";

  # yabai    => ~/Library/LaunchAgents/
  #          -> gui/$UID/org.nixos.yabai
  # yabai-sa => /Library/LaunchDaemons/
  #          -> system/org.nixos.yabai-sa
  daemonPath = "/Library/LaunchDaemons/org.nixos.yabai-sa.plist";

  defaultPadding = "6";

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

  # FIXME: avoid IFD -- add to a package derivation
  mkScriptFromFile = name: (writeScriptBin "yabai-${name}"
    (builtins.readFile "${configDir}/bin/${name}"));

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

        PADDING="''${1:-''${YABAI_PADDING_DEFAULT:-${defaultPadding}}}"

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
  environment.systemPackages =
    [yabaiPackage]
    ++ (map (key: builtins.getAttr key scripts) (builtins.attrNames scripts));

  environment.variables = {
    YABAI_PADDING_DEFAULT = defaultPadding;
  };

  # FIXME: scoping these to a user path isn't possible without assumptions about the username
  # launchd.user.agents.yabai.serviceConfig = {
  #   StandardOutPath = "${config.my.xdg.cache}/yabai.out.log";
  #   StandardErrorPath = "${config.my.xdg.cache}/yabai.err.log";
  # };

  services.yabai = {
    enable = true;
    package = yabaiPackage;
    enableScriptingAddition = true;

    config = {
      ###: --- tiling options ---

      layout = "bsp";
      window_placement = "second_child";

      top_padding = defaultPadding;
      bottom_padding = defaultPadding;
      left_padding = defaultPadding;
      right_padding = defaultPadding;
      window_padding = defaultPadding;

      auto_balance = "on";
      split_ratio = 0.5;

      ###: --- mouse support ---

      mouse_follows_focus = "off";
      focus_follows_mouse = "off"; # <- "autoraise" | "autofocus"
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";

      ###: --- window modifications ---

      window_topmost = "off";
      window_shadow = "float";
      window_opacity = "off";

      ###: --- status bar ---

      external_bar = false;
    };

    extraConfig = let
      commonRules = {
        manage = false;
        sticky = false;
      };

      rules = mkRules [
        (commonRules // {app = "1Password";})
        (commonRules // {app = "Alfred Preferences";})
        (commonRules // {app = "AppCleaner";})
        (commonRules // {app = "Fanatastical Helper";})
        (commonRules // {app = "Stickies";})
        (commonRules // {app = "^System Preferences$";})

        # Prevent tiny file copy dialogs from claiming a space partition.
        (commonRules
          // {
            app = "^Finder$";
            title = "Copy";
          })

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

        #: 'home' :: personal browser etc.
        yabai -m space 1 --label 'home'

        #: 'work' :: work browser + zoom
        yabai -m space 2 --label 'work'

        #: 'iterate' :: additional browser, other utils for testing current task
        yabai -m space 3 --label 'test'

        #: 'code' :: emacs, sometimes kitty
        yabai -m space 4 --label 'code'

        #: 'term' :: kitty
        yabai -m space 5 --label 'term'

        #: 'media' :: audio/video player, e.g. plexamp, spotify, youtube, vlc
        yabai -m space 6 --label 'media'

      ### }}}


      ###: DESKTOP WORKSPACES {{{
      # FIXME: declare target display

        ##: CENTER DISPLAY {{

          # yabai -m space 1 --label 'work'
          # yabai -m space 2 --label 'iterate'
          # yabai -m space 3 --label 'code'

        ## }}

        ##: RIGHT DISPLAY {{

          # yabai -m space 1 --label 'term'
          # yabai -m space 2 --label 'home'
          # yabai -m space 3 --label 'media'

        ## }}

      ### }}}

      ${rules}
      ${signals}
    '';
  };
}
