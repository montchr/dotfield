{ config, ... }:
let
  cfg = config.programs.eww;
in
{
  programs.eww.enable = true;
  home.packages = [ cfg.package ];
}
