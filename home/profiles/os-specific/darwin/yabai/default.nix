{
  config,
  inputs,
  self,
  pkgs,
  ...
}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit
    (inputs.apparat.lib.apps.yabai)
    toYabaiConfig
    mkSetting
    mkSignal
    mkSignal'
    mkRules
    ;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
  configSrcBasePath = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
  loadScriptingAddition = "sudo yabai --load-sa";
  defaultPadding = "6";
  rules = import ./rules.nix;
in
  l.mkIf isDarwin {
    home.sessionVariables = {
      YABAI_PADDING_DEFAULT = defaultPadding;
    };
    xdg.configFile."skhd".source = mkOutOfStoreSymlink "${configSrcBasePath}/skhd";

    xdg.configFile."yabai/yabairc".text = l.concatLines [
      # Ensure the scripting addition is loaded.
      (mkSignal' "dock_did_restart" loadScriptingAddition)
      loadScriptingAddition
      (toYabaiConfig {
        layout = "bsp";
        window_placement = "second_child";
        auto_balance = "off";
        split_ratio = 0.5;

        top_padding = defaultPadding;
        bottom_padding = defaultPadding;
        left_padding = defaultPadding;
        right_padding = defaultPadding;
        window_gap = defaultPadding;
        external_bar = false;

        #: mouse support
        mouse_follows_focus = "on";
        focus_follows_mouse = "off"; # <- "autoraise" | "autofocus" | "off"
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";

        #: window modifications
        window_topmost = "off";
        window_shadow = "float";

        #: opacity
        window_opacity = "on";
        active_window_opacity = 1.0;
        normal_window_opacity = 0.98;
      })
      ''
        ###: INITIALIZE ==============================================================================

        # Set window padding to default value.
        ${l.concatMapStrings (v: mkSetting v "6") [
          "top_padding"
          "bottom_padding"
          "left_padding"
          "right_padding"
          "window_gap"
        ]}

        ###: RULES ===================================================================================

        ${mkRules rules}

        ###: SIGNALS =================================================================================

        ${mkSignal "window_focused" "yabai -m query --windows --window" {
          label = "log each focused window";
        }}
      ''
    ];
  }
