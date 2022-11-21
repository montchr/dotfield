{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (builtins) toString;
  inherit (inputs) nix-colors;
  inherit
    (lib)
    concatMapStringsSep
    concatStringsSep
    isBool
    mkIf
    optionalString
    ;
  inherit (lib.generators) toKeyValue;
  inherit (lib.hm.booleans) yesNo;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config.dotfield.features) hasPragPro;
  inherit (config) theme;

  settings = import ./settings.nix {inherit lib;};
  colors = import ./colors.nix config.colorscheme;

  # via home-manager kitty module
  toKittyConfig = toKeyValue {
    mkKeyValue = key: value: let
      value' =
        if isBool value
        then (yesNo value)
        else toString value;
    in "${key} ${value'}";
  };

  colorScheme = name: import ./colors.nix nix-colors.colorSchemes.${name};
  mkColorScheme = name: toKittyConfig (colorScheme name);

  mkFontFeatures = name: features: "font_features ${name} ${concatStringsSep " " features}";

  mkFontFeatures' = family: styles: features:
    concatMapStringsSep "\n"
    (style: mkFontFeatures "${family}-${style}" features)
    styles;

  iosevkaPostScriptSuffixes = ["Bold" "Bold-Extended" "Bold-Extended-Italic" "Bold-Extended-Oblique" "Bold-Italic" "Bold-Oblique" "Extended" "Extrabold" "Extralight" "Heavy" "Italic" "Light" "Medium" "Medium-Italic" "Regular" "Semibold" "Thin" "Thin-Italic"];
  fontSettings = {
    font_family = theme.fonts.term.family;
    font_size = "${builtins.toString theme.fonts.term.size}.0";
  };
in {
  home.packages = with pkgs; [
    kitty-get-window-by-platform-id
  ];

  home.sessionVariables = {
    KITTY_CONFIG_DIRECTORY = "${config.xdg.configHome}/kitty";
  };

  programs.kitty = {
    enable = true;
    settings = settings // colors // fontSettings;
    darwinLaunchOptions = mkIf isDarwin ["--single-instance"];
    extraConfig = mkFontFeatures' "Iosevka-Term" iosevkaPostScriptSuffixes ["-calt +dlig"];
  };

  xdg.configFile = {
    "kitty/themes".source = ./themes;

    # "kitty/themes/dark.conf".source = ./themes/dark/Substrata.conf;
    # "kitty/themes/light.conf".source = ./themes/light/Alabaster.conf;

    # FIXME: allow for specifying custom nix-colors-compatible color schemes
    # "kitty/themes/dark.conf".text = mkColorScheme theme.colors.dark;
    # "kitty/themes/light.conf".text = mkColorScheme theme.colors.light;

    # FIXME: does not appear to have an effect?
    "kitty/session".text = ''
      # Start new sessions in the previous working directory
      # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
      # https://sw.kovidgoyal.net/kitty/faq/#how-do-i-open-a-new-window-or-tab-with-the-same-working-directory-as-the-current-window
      launch --cwd=current
    '';
  };
}
