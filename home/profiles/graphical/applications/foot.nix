## ISSUES
#
# - Cursor scaling broken in GNOME, leading to gigantic cursor: <https://codeberg.org/dnkl/foot/issues/1426>
{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  themeCfg = config.theme;
  themeFont = themeCfg.font.terminal;
in
{
  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    programs.foot = {
      enable = true;
      settings = {

        main = {
          # TODO: set `initial-window-size-pixels` based on display
          initial-window-size-chars = "100x30";
          initial-window-mode = "windowed";
          pad = "10x8";
        };

        bell = {
          visual = true;
        };

        scrollback = {
          lines = 10000;
        };

        cursor = {
          blink = true;
          style = "beam";
        };

        mouse = {
          hide-when-typing = true;
        };

        key-bindings = {
          # Avoid conflict with shell history binding C-r
          search-start = "Control+Shift+r";
        };
      };
    };
  };
}
