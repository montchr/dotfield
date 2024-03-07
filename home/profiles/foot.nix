{ config, pkgs, ... }:
let
  themeCfg = config.theme;
in
{
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "${themeCfg.fonts.terminal.name}:size=${themeCfg.fonts.terminal.size}";
        dpi-aware = true;
        include = [ "${pkgs.foot.themes}/modus-operandi" ];
      };
      cursor.blink = true;
      cursor.style = "beam";
    };
  };
}
