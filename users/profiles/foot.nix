{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "PragmataPro Liga:size=13";
        dpi-aware = true;
      };
      cursor.blink = true;
      cursor.style = "beam";
    };
  };
}
