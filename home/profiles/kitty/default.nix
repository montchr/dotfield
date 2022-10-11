{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (builtins) toString;
  inherit (inputs) base16-kitty nix-colors;
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
  inherit (config.lib.dotfield) features;
  inherit (config) theme;

  socket = "unix:/tmp/kitty-socket";

  settings = import ./settings.nix {inherit lib features socket;};
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

  pragmataProExtras = let
    fontStyles = ["Regular" "Italic" "BoldItalic"];
    # https://fsd.it/pragmatapro/Handbook.png
    fontFeatures = [
      "+calt" # Standard programming ligatures.
      # "+frac" # Fraction stylization.
      # "+ss12" # Assorted iconizations.
      # "+ss13" # Smooth graphs e.g. for git log.
    ];
  in ''
    ${mkFontFeatures' "PragmataProMono" fontStyles fontFeatures}
  '';
in {
  home.packages = with pkgs; [
    kitty-get-window-by-platform-id
    # (mkIf isDarwin kitty-set-app-icon)
  ];

  programs.kitty = {
    enable = true;
    settings =
      settings
      // colors
      // {
        font_family = theme.fonts.term.family;
        font_size = "${builtins.toString theme.fonts.term.size}.0";
      };
    extraConfig = ''
      ${optionalString features.hasPragPro pragmataProExtras}
    '';

    darwinLaunchOptions = mkIf isDarwin [
      "--single-instance"
      "--listen-on=${socket}"
    ];
  };

  xdg.configFile = {
    "kitty/base16-kitty".source = base16-kitty.outPath;
    "kitty/themes/dark.conf".text = mkColorScheme theme.colors.dark;
    "kitty/themes/light.conf".text = mkColorScheme theme.colors.light;

    # FIXME: does not appear to have an effect?
    "kitty/session".text = ''
      # Start new sessions in the previous working directory
      # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
      # https://sw.kovidgoyal.net/kitty/faq/#how-do-i-open-a-new-window-or-tab-with-the-same-working-directory-as-the-current-window
      launch --cwd=current
    '';
  };
}
