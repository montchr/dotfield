{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.lib.our) mkOpt;
  inherit (pkgs.lib.types) int str;
in {
  options = {
    theme = {
      enable = lib.mkEnableOption "Whether to enable the theme module.";
      font = {
        mono = {
          family = mkOpt str "";
          weight = mkOpt int 400;
          size = mkOpt int 13;
        };
        sans = {
          family = mkOpt str "";
          weight = mkOpt int 400;
          size = mkOpt int 10;
        };
        emoji.family = mkOpt str "";
      };
    };
  };
}
