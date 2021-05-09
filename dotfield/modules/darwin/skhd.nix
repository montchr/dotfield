{ config, lib, options, ... }:

let cfgDir = "${config.dotfield.configDir}/skhd";
in {
  options = with lib; {
    my.modules.skhd = {
      enable = mkEnableOption ''
        Whether to enable skhd module
      '';
    };
  };

  config = {
    services.skhd = { enable = true; };

    my.hm.configFile."skhd/skhdrc".source = "${cfgDir}/skhdrc";
  };
}
