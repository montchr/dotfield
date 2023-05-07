{config, ...}: let
  themeCfg = config.theme;
in {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "${themeCfg.fonts.term.family}:size=${themeCfg.fonts.term.size}";
        dpi-aware = true;
      };
      cursor.blink = true;
      cursor.style = "beam";
    };
  };
}
