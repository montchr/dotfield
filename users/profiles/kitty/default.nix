{ config, lib, options, pkgs, inputs, ... }:

let
  inherit (inputs) base16-kitty nix-colors;
  inherit (config) my colorscheme;
  inherit (colorscheme) colors;

  configDir = "${config.dotfield.configDir}/kitty";
  socket = "unix:/tmp/kitty-socket";

  kitty-get-window-by-platform-id =
    (pkgs.writeShellScriptBin "kitty-get-window-by-platform-id" ''
      kitty @ --to $KITTY_SOCKET ls \
        | ${pkgs.jq}/bin/jq -r --argjson id "$1" \
          '.[] | select(.platform_window_id==$id)'
    '');

  # via home-manager kitty module
  toKittyConfig = lib.generators.toKeyValue {
    mkKeyValue = key: value:
      let
        value' =
          if lib.isBool value then
            (if value then "yes" else "no")
          else
            builtins.toString value;
      in
      "${key} ${value'}";
  };

  mkTheme = name: (import ./colors.nix {
    inherit (nix-colors.colorSchemes.${name}) colors;
  });

  mkTheme' = name: ''
    # ${my.nix_managed}

    ${toKittyConfig (mkTheme name)}
  '';

  mkFontFeatures = name: features:
    "font_features ${name} ${lib.concatStringsSep " " features}";

  mkFontFeatures' = family: styles: features:
    lib.concatMapStringsSep "\n"
      (style: mkFontFeatures "${family}-${style}" features)
      styles;

in

lib.mkMerge [

  (lib.optionalAttrs (builtins.hasAttr "homebrew" options) {
    homebrew.casks = [ "kitty" ];
  })

  {
    my.user.packages = [
      # TODO: enable for linux
      # pkgs.kitty
      kitty-get-window-by-platform-id
    ];

    my.hm.home.sessionVariables = {
      KITTY_CONFIG_DIRECTORY = "${my.xdg.config}/kitty";
      KITTY_SOCKET = socket;
      TERMINFO_DIRS =
        if pkgs.stdenv.isDarwin then
          "/Applications/kitty.app/Contents/Resources/kitty/terminfo"
        else
          "${pkgs.kitty.terminfo.outPath}/share/terminfo";
    };

    my.hm.programs.kitty = {
      enable = true;

      darwinLaunchOptions = [
        "--single-instance"
        "--listen-on=${socket}"
      ];

      extraConfig =
        let
          fontStyles = [ "Regular" "Italic" "BoldItalic" ];
          # https://fsd.it/pragmatapro/Handbook.png
          fontFeatures = [
            "+calt" # Standard programming ligatures.
            # "+frac" # Fraction stylization.
            # "+ss12" # Assorted iconizations.
            "+ss13" # Smooth graphs e.g. for git log.
          ];
        in
        ''
          ${mkFontFeatures' "PragmataProMonoLiga" fontStyles fontFeatures}
        '';

      settings =
        (import ./settings.nix { inherit config lib; }) //
        (mkTheme config.colorscheme.slug) //
        {
          allow_remote_control = "yes";
          listen_on = socket;
        };
    };

    my.hm.xdg.configFile = {
      "kitty/base16-kitty".source = base16-kitty.outPath;
      "kitty/session".text = "cd ~";
      "kitty/themes/dark.conf".text = mkTheme' "black-metal-khold";
      "kitty/themes/light.conf".text = mkTheme' "grayscale-light";
    };

  }
]
