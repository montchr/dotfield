{
  config,
  lib,
  pkgs,
  ...
}: lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "Iosevka Term:size=13";
        dpi-aware = true;
      };
      cursor.blink = true;
      cursor.style = "beam";
    };
  };
}
