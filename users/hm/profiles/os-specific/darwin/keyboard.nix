{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  configDir = ../../../../../config;
in {
  services.skhd.enable = true;
  services.skhd.config = builtins.readFile "${configDir}/skhd/skhdrc";

  xdg.configFile."karabiner/karabiner.json".source = "${configDir}/karabiner/karabiner.json";
}
