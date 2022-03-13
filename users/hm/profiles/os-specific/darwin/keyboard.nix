{ config, osConfig, lib, pkgs, ... }:

let
  inherit (osConfig.dotfield) configDir;
in

{
  services.skhd.enable = true;
  services.skhd.config = builtins.readFile "${configDir}/skhd/skhdrc";

  xdg.configFile."karabiner/karabiner.json".source = "${configDir}/karabiner/karabiner.json";
}
