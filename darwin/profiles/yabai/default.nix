# FIXME: migrate off nix-darwin module since we don't need it aside from the config-builder
{
  pkgs,
  self,
  inputs,
  config,
  ...
}: let
  inherit (pkgs) writeScriptBin writeShellScriptBin;
  inherit
    (self.lib.apps.yabai)
    toYabaiConfig
    mkSetting
    mkSignal
    mkSignal'
    mkRules
    defineSpaces
    ;

  l = inputs.nixpkgs.lib // builtins;
  cfg = config.services.yabai;

  configBasePath = self + "/home/users/cdom/config";
  configDir = "${configBasePath}/yabai";

  rules = import ./rules.nix;

  defaultPadding = "6";

  # TODO: package these up
  mkScriptFromFile = name: (writeScriptBin "yabai-${name}"
    (l.readFile "${configDir}/bin/${name}"));

  scriptsFromFiles = l.map mkScriptFromFile [
    "close-window"
    "focus-direction"
  ];

  scripts =
    (l.listToAttrs (l.map
      (drv: {
        inherit (drv) name;
        value = drv;
      })
      scriptsFromFiles))
    // {
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

        ${l.concatMapStrings (v: mkSetting v "$PADDING") [
          "top_padding"
          "bottom_padding"
          "left_padding"
          "right_padding"
          "window_gap"
        ]}
      '';
    };

  # Get the store path to a yabai script by shortname.
  getScript = n: "${l.getAttr n scripts}/bin/yabai-${n}";
in {
  environment.systemPackages = l.map (key: l.getAttr key scripts) (l.attrNames scripts);

  environment.variables = {
    YABAI_PADDING_DEFAULT = defaultPadding;
  };

  # FIXME: not actually loaded by yabai -- emergency symlinked to my xdgconfighome
  environment.etc."yabai/yabairc".text = l.concatLines [(toYabaiConfig cfg.config) cfg.extraConfig];

  homebrew.taps = ["koekeishiya/formulae"];

  homebrew.brews = [
    {
      name = "yabai";
      start_service = true;
      restart_service = true;
      link = true;
      # NOTE: sometimes may be necessary for experimental support of new macOS versions
      #       if installing from HEAD, make sure to codesign!
      #       see <https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(from-HEAD)>
      # args = ["HEAD"];
    }
    {
      name = "skhd";
      args = ["with-logging"];
      start_service = true;
      restart_service = true;
      link = true;
    }
  ];

  services.yabai = {
    enable = true;
    # NOTE: nix-darwin does NOT manage the yabai package!
    #       nix-managed yabai/skhd does not support building HEAD from source.
    #       i've run into so many issues that i don't ever want to deal with again,
    #       so just "cheat" and install with brew.
    #       life is short.
    package = pkgs.runCommand "yabai-0.0.0" {} "mkdir $out";

    # NOTE: Again, we do not want nix-darwin handling anything other then config building.
    enableScriptingAddition = false;

    config = {
      layout = "bsp";
      window_placement = "second_child";
      auto_balance = "off";
      split_ratio = 0.5;

      top_padding = defaultPadding;
      bottom_padding = defaultPadding;
      left_padding = defaultPadding;
      right_padding = defaultPadding;
      window_padding = defaultPadding;
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
    };

    extraConfig = ''
      ###: INITIALIZE ==============================================================================

      # Ensure the scripting addition is loaded.
      ${mkSignal' "dock_did_restart" "sudo yabai --load-sa"}

      # Set window padding to default value.
      ${getScript "set-padding"}

      ###: RULES ===================================================================================

      ${mkRules rules}

      ###: SIGNALS =================================================================================

      ${mkSignal "window_focused" "yabai -m query --windows --window" {
        label = "log each focused window";
      }}

      ###: WORKSPACES ==============================================================================

      ${defineSpaces [
        "home" #   -> personal browser etc.
        "work" #   -> work browser + zoom
        "test" #   -> additional browser, other utils for testing current task
        "code" #   -> editor, sometimes terminal
        "term" #   -> terminal
        "media" #  -> audio/video player, e.g. plexamp, spotify, youtube, vlc
      ]}
    '';
  };
}
