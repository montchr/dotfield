moduleArgs @ {
  options,
  config,
  inputs,
  ...
}: let
  inherit (l.types) str int;
  inherit (inputs) apparat nix-colors;
  inherit (apparat.lib) mkOpt;

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

  normalWeight = 400;
in {
  options = {
    theme = {
      enable = l.mkEnableOption "Whether to enable the theme module.";
      colors = let
        colorSchemeOptions = options.colorScheme;
        colorSchemeType = l.types.submodule {options = colorSchemeOptions;};
        mkColorSchemeOption = kind:
          l.mkOption {
            default = nix-colors.colorSchemes."default-${kind}";
            type = colorSchemeType;
          };
      in {
        active = l.mkOption {
          type = l.types.nullOr colorSchemeType;
          default = null;
          description = ''
            Currently-active color scheme.
          '';
        };
        dark = mkColorSchemeOption "dark";
        light = mkColorSchemeOption "light";
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
    colorScheme =
      if (cfg.colors.active != null)
      then cfg.colors.active
      else cfg.colors.dark;
  };
}
