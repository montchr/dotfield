{ pkgs, lib, config, options, inputs, ... }:
with lib;
let
  inherit (inputs) base16-kitty;

  cfg = config.my.modules.kitty;
  themesCfg = config.my.modules.themes;
  configDir = "${config.dotfield.configDir}/kitty";
  socket = "unix:/tmp/kitty-socket";

  # Create a Kitty config string from a Nix set.
  # https://github.com/nix-community/home-manager/blob/b0d769691cc379c9ab91d3acec5d14e75c02c02b/modules/programs/kitty.nix#L14-L22
  toKittyConfig = generators.toKeyValue {
    mkKeyValue = key: value:
      let
        value' =
          if isBool value then
            (if value then "yes" else "no")
          else
            toString value;
      in
      "${key} ${value'}";
  };

  # Create kitty keybindings from a set.
  # https://github.com/nix-community/home-manager/blob/59be1f4983ee3689de3172716a6c7e95a6a37bb7/modules/programs/kitty.nix#L24-L26
  toKittyKeybindings = generators.toKeyValue {
    mkKeyValue = key: command: "map ${key} ${command}";
  };

  # Write a Nix set representing a kitty config into the Nix store.
  writeKittyConfig = fileName: config:
    pkgs.writeTextDir "${fileName}" (toKittyConfig config);

  # Path in Nix store containing light and dark kitty color configs
  # kitty-colors = pkgs.symlinkJoin {
  #   name = "kitty-colors";
  #   paths = [
  #     (writeKittyConfig "dark-colors.conf" cfg.colors.dark)
  #     (writeKittyConfig "light-colors.conf" cfg.colors.light)
  #   ];
  # };

  # Shell scripts for changing Kitty colors
  # term-colors = pkgs.writeShellScriptBin "term-colors" ''
  #   # Accepts arguments "light" or "dark". If shell is running in a Kitty window set the colors.
  #   if [[ -n "$KITTY_WINDOW_ID" ]]; then
  #     kitty @ --to $KITTY_LISTEN_ON set-colors --all --configured \
  #       ${kitty-colors}/"$1"-colors.conf &
  #   fi
  # '';
  # term-light = pkgs.writeShellScriptBin "term-light" ''
  #   ${term-colors}/bin/term-colors light
  # '';
  # term-dark = pkgs.writeShellScriptBin "term-dark" ''
  #   ${term-colors}/bin/term-colors dark
  # '';

  kitty-get-window-by-platform-id =
    (pkgs.writeShellScriptBin "kitty-get-window-by-platform-id" ''
      kitty @ --to $KITTY_SOCKET ls \
        | ${pkgs.jq}/bin/jq -r --argjson id "$1" \
          '.[] | select(.platform_window_id==$id)'
    '');
in
{
  options = with lib; {
    my.modules.kitty = {
      enable = mkEnableOption ''
        Whether to enable kitty module
      '';

    } // (import ./options.nix { inherit config lib pkgs; });
  };

  config = with lib;
    mkIf cfg.enable (mkMerge [{
      my.user.packages = [
        # pkgs.kitty
        kitty-get-window-by-platform-id
        # term-colors
        # term-light
        # term-dark
      ];

      environment.variables = {
        KITTY_CONFIG_DIRECTORY = "$XDG_CONFIG_HOME/kitty";
        KITTY_SOCKET = "unix:$XDG_RUNTIME_DIR/kitty-socket";
        TERMINFO_DIRS =
          if pkgs.stdenv.isDarwin then
            "/Applications/kitty.app/Contents/Resources/kitty/terminfo"
          else
          # FIXME causes build failure on darwin due to beautifulsoup unit test failure (but it should not fail)
            "${pkgs.kitty.terminfo.outPath}/share/terminfo";
      };

      my.modules.kitty = {
        settings = (import ./settings.nix {
          inherit config lib;
        });
        extraConfig = ''
          font_features PragmataProMonoLiga-Regular +calt
          font_features PragmataProMonoLiga-Italic +calt
          font_features PragmataProMonoLiga-BoldItalic +calt

          # TODO: remove once kitty module supports base16-nix
          include ./theme.conf
        '';
      };

      my.hm.xdg.configFile = {
        "kitty/base16-kitty".source = base16-kitty.outPath;

        "kitty/kitty.conf".text = ''
          # ${config.my.nix_managed}
          # See https://sw.kovidgoyal.net/kitty/conf.html

          ${toKittyConfig cfg.settings}
          ${toKittyKeybindings cfg.keybindings}
          ${cfg.extraConfig}
        '';

        # TODO: why is `--listen-on` necessary instead of setting the
        # associated setting?
        "kitty/macos-launch-services-cmdline".text =
          mkIf (pkgs.stdenv.isDarwin) ''
            --single-instance
            --listen-on ${socket}
          '';

        "kitty/session".text = "cd ~";
      };

      # settings = optionalAttrs cfg.colors.enable (

      #   cfg.colors.common // cfg.colors.${cfg.colors.default} // {
      #     allow_remote_control = "yes";
      #     listen_on = socket;
      #   }

      # );

    }]);
}
