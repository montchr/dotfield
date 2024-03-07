{ config, ... }:
let
  inherit (config) xdg;
in
{
  programs.zoxide.enable = true;
  home.sessionVariables."_ZO_DATA_DIR" = xdg.dataHome;
}
