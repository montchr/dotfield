{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.lib.our) mkOpt;
  inherit (pkgs.lib.types) int str;

  cfg = config.theme;
  normalWeight = 400;
in {
  options = {
    theme = {
      enable = lib.mkEnableOption "Whether to enable the theme module.";
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
        # FIXME: ?
        emoji.family = mkOpt str "";
      };
    };
  };
}
