{ pkgs, config, ... }:
{
  home.packages = [ pkgs.mako ];
  services.mako = {
    enable = true;
    anchor = "top-center";
    font = "monospace 8";
    icons = true;
  };

}
