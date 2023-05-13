{
  config,
  lib,
  ...
}: let
  inherit (config) theme;
in {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = lib.mkIf theme.enable (
          "${theme.fonts.term.family}"
          + ":size=${theme.fonts.term.size}"
        );
        dpi-aware = true;
      };
      cursor.blink = true;
      cursor.style = "beam";
    };
  };
}
