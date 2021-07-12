{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.my.modules.php;
  composerConfigDir = "${config.dotfield.configDir}/composer";
in {
  options = with lib; { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    my.user.packages = with pkgs; [ php php74Packages.composer ];

    my.hm.configFile = {
      "composer/composer.json" = "${composerConfigDir}/composer.json";
      "composer/composer.lock" = "${composerConfigDir}/composer.lock";
      };
    };
  };
}
