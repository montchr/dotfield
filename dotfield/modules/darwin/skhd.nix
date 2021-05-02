{ config, home-manager, ... }:

let

  cfgDir = "${config.dotfield.configDir}/skhd";

in {
  home-manager.services.skhd = {
    enable = true;
    skhdConfig = builtins.readFile builtins.toPath /. "${cfgDir}/skhdrc";
  };
}
