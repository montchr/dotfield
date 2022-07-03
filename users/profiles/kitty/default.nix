moduleArgs @ {
  config,
  lib,
  options,
  pkgs,
  inputs,
  ...
}: let
  inherit (inputs) base16-kitty nix-colors;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  hasTwm = moduleArgs.osConfig.services.yabai.enable or config.wayland.windowManager.sway.enable;
  socket = "unix:/tmp/kitty-socket";

  # via home-manager kitty module
  toKittyConfig = lib.generators.toKeyValue {
    mkKeyValue = key: value: let
      value' =
        if lib.isBool value
        then (lib.hm.booleans.yesNo value)
        else builtins.toString value;
    in "${key} ${value'}";
  };

  mkTheme = name: import ./colors.nix nix-colors.colorSchemes.${name};
  mkTheme' = name: toKittyConfig (mkTheme name);

  mkFontFeatures = name: features: "font_features ${name} ${lib.concatStringsSep " " features}";

  mkFontFeatures' = family: styles: features:
    lib.concatMapStringsSep "\n"
    (style: mkFontFeatures "${family}-${style}" features)
    styles;
in
  lib.mkMerge [
    (lib.mkIf isLinux {
      home.packages = [pkgs.kitty];
    })

    (lib.mkIf isDarwin {
      # Handled by the Homebrew module
      # This populates a dummy package to satisfy the requirement
      programs.kitty.package = pkgs.runCommand "kitty-0.0.0" {} "mkdir $out";

      programs.kitty.darwinLaunchOptions = [
        "--single-instance"
        "--listen-on=${socket}"
      ];
    })

    {
      home.packages = with pkgs; [
        kitty-helpers.getWindowByPlatformId
        (lib.mkIf isDarwin kitty-helpers.setAppIcon)
      ];

      home.sessionVariables = {
        KITTY_CONFIG_DIRECTORY = "${config.xdg.configHome}/kitty";
        # FIXME: necessary?
        KITTY_SOCKET = socket;
      };

      programs.kitty = {
        enable = true;

        extraConfig = let
          fontStyles = ["Regular" "Italic" "BoldItalic"];
          # https://fsd.it/pragmatapro/Handbook.png
          fontFeatures = [
            "+calt" # Standard programming ligatures.
            # "+frac" # Fraction stylization.
            # "+ss12" # Assorted iconizations.
            "+ss13" # Smooth graphs e.g. for git log.
          ];
        in ''
          ${mkFontFeatures' "PragmataProMonoLiga" fontStyles fontFeatures}
        '';

        settings =
          (import ./settings.nix {inherit lib hasTwm socket;})
          // (import ./colors.nix config.colorscheme);
      };

      xdg.configFile = {
        "kitty/base16-kitty".source = base16-kitty.outPath;
        "kitty/themes/dark.conf".text = mkTheme' "black-metal-khold";
        "kitty/themes/light.conf".text = mkTheme' "grayscale-light";

        "kitty/session".text = ''
          # Start new sessions in the previous working directory
          # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
          # https://sw.kovidgoyal.net/kitty/faq/#how-do-i-open-a-new-window-or-tab-with-the-same-working-directory-as-the-current-window
          launch --cwd=current
        '';
      };
    }
  ]
