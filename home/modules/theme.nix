moduleArgs @ {
  options,
  config,
  inputs,
  self,
  ...
}: let
  inherit (l.types) str int;
  inherit (inputs.flib.lib.options) mkOpt;
  inherit (self.lib.colors) getColorScheme;
  inherit (l) types;
  l = inputs.nixpkgs.lib // builtins;
  cfg = config.theme;

  # Single-item list format follows the NixOS options.
  defaultFonts = let
    fonts =
      moduleArgs.osConfig.fonts.fontconfig.defaultFonts
      or {
        monospace = ["DejaVu Sans Mono"];
        sansSerif = ["DejaVu Sans"];
        serif = ["DejaVu Serif"];
      };
  in
    l.mapAttrs (_: l.head) fonts;

  colorSchemeType = l.types.submodule {options = options.colorScheme;};
  mkColorSchemeOption = default:
    l.mkOption {
      inherit default;
      type = colorSchemeType;
    };

  normalWeight = 400;
in {
  options = {
    theme = {
      enable = l.mkEnableOption "Whether to enable the theme module.";
      colors = {
        active = l.mkOption {
          type = with types; nullOr colorSchemeType;
          default = cfg.colors.dark;
          description = ''
            Currently-active color scheme, defaulting to the value of the dark theme.
          '';
        };
        dark = mkColorSchemeOption (getColorScheme "default-dark");
        light = mkColorSchemeOption (getColorScheme "default-light");
      };
      fonts = {
        mono = {
          family = mkOpt str defaultFonts.monospace;
          weight = mkOpt int normalWeight;
          size = mkOpt int 13;
        };
        term = with cfg.fonts; {
          family = mkOpt str mono.family;
          weight = mkOpt int mono.weight;
          size = mkOpt int mono.size;
        };
        sans = {
          family = mkOpt str defaultFonts.sansSerif;
          weight = mkOpt int normalWeight;
          size = mkOpt int 10;
        };
        serif = {
          family = mkOpt str defaultFonts.serif;
          weight = mkOpt int normalWeight;
          size = mkOpt int cfg.fonts.sans.size;
        };
        emoji.family = mkOpt str "";
        symbols.family = mkOpt str "";
      };
    };
  };

  config = l.mkIf cfg.enable {
    colorScheme = cfg.colors.active;
  };
}
