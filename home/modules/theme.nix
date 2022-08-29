{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (inputs.nix-colors) colorSchemes;
  inherit (lib.dotfield) mkOpt;
  inherit (lib.types) int str;

  cfg = config.theme;

  normalWeight = 400;
in {
  options = {
    theme = {
      enable = lib.mkEnableOption "Whether to enable the theme module.";
      colors = {
        active = mkOpt str cfg.colors.dark;
        # FIXME: set a sensible default
        dark = mkOpt str "";
        # FIXME: set a sensible default
        light = mkOpt str "";
      };
      font = {
        mono = {
          # FIXME: set a sensible default
          family = mkOpt str "";
          weight = mkOpt int normalWeight;
          size = mkOpt int 13;
        };
        sans = {
          # FIXME: set a sensible default
          family = mkOpt str "";
          weight = mkOpt int normalWeight;
          size = mkOpt int 10;
        };
        serif = {
          # FIXME: set a sensible default
          family = mkOpt str "";
          weight = mkOpt int normalWeight;
          size = mkOpt int cfg.font.sans.size;
        };
        emoji.family = mkOpt str "";
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
