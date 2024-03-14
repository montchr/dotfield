{
  config,
  flake,
  pkgs,
  ...
}:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (flake.inputs.apparat.lib.yabai)
    toYabaiConfig
    mkSetting
    mkSignal'
    ;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
  configSrcBasePath = "${config.xdg.configHome}/dotfield/users/cdom/config";
  loadScriptingAddition = "sudo yabai --load-sa";
  defaultPadding = "6";
in
l.mkIf isDarwin {
  home.sessionVariables = {
    YABAI_PADDING_DEFAULT = defaultPadding;
  };
  xdg.configFile."skhd".source = mkOutOfStoreSymlink "${configSrcBasePath}/skhd";

  xdg.configFile."yabai/yabairc".executable = true;
  xdg.configFile."yabai/yabairc".text = l.concatLines [
    # Ensure the scripting addition is loaded.
    loadScriptingAddition
    (mkSignal' "dock_did_restart" loadScriptingAddition)

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
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      # mouse_follows_focus = "on";
      # focus_follows_mouse = "off"; # <- "autoraise" | "autofocus" | "off"

      #: window modifications
      # window_shadow = "float";

      #: opacity
      window_opacity = "on";
      active_window_opacity = 1.0;
      normal_window_opacity = 0.98;
    })
    ''
      ###: INITIALIZE ============================================================================

      # Set window padding to default value.
      ${l.concatMapStrings (v: mkSetting v defaultPadding) [
        "top_padding"
        "bottom_padding"
        "left_padding"
        "right_padding"
        "window_gap"
      ]}

      ###: FUNCTIONS =============================================================================

      # via <https://github.com/d12frosted/environment/blob/48774588094fd2b1a10cf721eaaf5697f5f7a3d7/nix/darwin/yabai.nix>
      function setup_space {
        local idx="$1"
        local name="$2"
        shift 2
        local rest=$@

        local space=

        echo "setup space $idx : $name"

        space=$(yabai -m query --spaces --space "$idx")
        if [ -z "$space" ]; then
          yabai -m space --create
        fi

        yabai -m space "$idx" --label "$name"
      }

      ###: SPACES =================================================================================

      # Restrict the maximum number of spaces, destroying extra spaces.
      # TODO: accept variable (why 6/7?)
      for _ in $(yabai -m query --spaces | jq '.[].index | select(. > 6)'); do
        yabai -m space --destroy 7
      done

      setup_space 1 web
      setup_space 2 dev
      setup_space 3 term
      setup_space 4 emacs
      setup_space 5 media
      setup_space 6 social

      ###: RULES =================================================================================

      yabai -m rule --add app="^1Password(\s\d)?$" manage=off
      yabai -m rule --add app="^Alfred Preferences$" manage=off
      yabai -m rule --add app="^AppCleaner$" manage=off
      yabai -m rule --add app="^Fantastical Helper$" manage=off
      yabai -m rule --add app="^Finder$" manage=off
      yabai -m rule --add app="^Stickies$" manage=off
      yabai -m rule --add app="^System (Settings|Preferences)$" manage=off

      # JetBrains apps do not work well with yabai due to many popup dialogs.
      # TODO: It may be possible to target just the dialogs.
      yabai -m rule --add app="^PhpStorm$" manage=off

      # NOTE: `mkRule` cannot handle negation
      # FIXME: expand on the above comment -- what is "handle" and "negation"?
      yabai -m rule --add app="^Emacs$" title!='^$' manage=on

      ##: Opacity and telepresence have a correlating relationship.
      yabai -m rule --add app="^Microsoft Teams$" opacity='1.0'
      yabai -m rule --add app="^zoom\.us$" opacity='1.0'

      ##: Move some apps automatically to specific spaces.
      # yabai -m rule --add app="^(Google )?Chrom(e|ium)$" space=2
      # yabai -m rule --add app="^Safari$" space=2
      yabai -m rule --add app="^(Visual Studio )?Code$" space=2
      yabai -m rule --add app="^Music$" space=5
      yabai -m rule --add app="^Spotify$" space=5 manage=off
      yabai -m rule --add app="^Signal$" space=6
      yabai -m rule --add app="^Messages$" space=6
      # NOTE: This rule *intentionally* does not target Firefox variants (e.g. ESR, Nightly, etc.)
      #       I tend to use Firefox Developer Edition for webdev tasks,
      #       and when doing webdev I tend to move windows around a lot.
      yabai -m rule --add app="^Firefox$" space=1
      # NOTE: Conversely, this rule applies to *all* Firefox variants.
      yabai -m rule --add app="^Firefox" title="^Extension:"

      ###: SIGNALS ===============================================================================

    ''
  ];
}
