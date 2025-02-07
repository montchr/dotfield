{ pkgs, config, ... }:
let
  theme = config.theme;
in
{
  home.packages = [ pkgs.mako ];
  services.mako = {
    enable = true;
    anchor = "top-center";
    layer = "overlay";
    defaultTimeout = 12000;
    font = "monospace 8";
    padding = "5,10";
    width = 300;
    height = 60;
    borderSize = 2;
    extraConfig = ''
      max-history=50
    '';
  };

}
