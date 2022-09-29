moduleArgs @ {
  config,
  lib,
  pkgs,
  inputs,
  self,
  ...
}: let
  inherit (builtins) head mapAttrs;
  inherit (inputs.nix-colors) colorSchemes;
  inherit (lib.types) int str;
  inherit (self.lib) mkOpt;

  cfg = config.theme;

  # The single-item list format of the fallback set follows that of the NixOS
  # option type. While repetitive, maybe, it's simple.
  defaultFonts = let
    fonts =
      moduleArgs.osConfig.fonts.fontconfig.defaultFonts
      or {
        monospace = ["DejaVu Sans Mono"];
        sansSerif = ["DejaVu Sans"];
        serif = ["DejaVu Serif"];
      };
  in
    mapAttrs (_: head) fonts;

  normalWeight = 400;
in {
  options = {
    theme = {
      enable = lib.mkEnableOption "Whether to enable the theme module.";
      colors = {
        active = mkOpt str cfg.colors.dark;
        dark = mkOpt str "default-dark";
        light = mkOpt str "default-light";
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

  config = lib.mkIf cfg.enable {
    home.sessionVariables = {
      BASE16_THEME_DARK = cfg.colors.dark;
      BASE16_THEME_LIGHT = cfg.colors.light;
    };
    colorScheme = colorSchemes.${cfg.colors.active};
  };
}
