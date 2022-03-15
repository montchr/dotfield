{
  config,
  lib,
  options,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;

  inherit (inputs) base16-kitty nix-colors;

  configDir = "${config.dotfield.configDir}/kitty";
  socket = "unix:/tmp/kitty-socket";

  kitty-get-window-by-platform-id = pkgs.writeShellScriptBin "kitty-get-window-by-platform-id" ''
    kitty @ --to $KITTY_SOCKET ls \
      | ${pkgs.jq}/bin/jq -r --argjson id "$1" \
        '.[] | select(.platform_window_id==$id)'
  '';

  # via home-manager kitty module
  toKittyConfig = lib.generators.toKeyValue {
    mkKeyValue = key: value: let
      value' =
        if lib.isBool value
        then
          (
            if value
            then "yes"
            else "no"
          )
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

    {
      home.packages = [
        kitty-get-window-by-platform-id
      ];

      home.sessionVariables = {
        KITTY_CONFIG_DIRECTORY = "${config.xdg.configHome}/kitty";
        KITTY_SOCKET = socket;
        TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
      };

      programs.kitty = {
        enable = true;

        darwinLaunchOptions = [
          "--single-instance"
          "--listen-on=${socket}"
        ];

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
          (import ./settings.nix {inherit config lib;})
          // (import ./colors.nix config.colorscheme)
          // {
            allow_remote_control = "yes";
            listen_on = socket;
          };
      };

      xdg.configFile = {
        "kitty/base16-kitty".source = base16-kitty.outPath;
        "kitty/session".text = "cd ~";
        "kitty/themes/dark.conf".text = mkTheme' "black-metal-khold";
        "kitty/themes/light.conf".text = mkTheme' "grayscale-light";
      };
    }
  ]
