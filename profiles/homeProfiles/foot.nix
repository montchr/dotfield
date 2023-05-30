{config, ...}: let
  themeCfg = config.theme;
in {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "${themeCfg.fonts.terminal.name}:size=${themeCfg.fonts.terminal.size}";
        dpi-aware = true;
      };
      cursor.blink = true;
      cursor.style = "beam";
    };
  };
}
