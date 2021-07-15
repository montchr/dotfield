{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.my.modules.php;
  composerConfigDir = "${config.my.xdgPaths.config}/composer";
in {
  options = { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    my.user.packages = with pkgs; [ php php74Packages.composer ];

    my.hm.configFile = with config.my.hm.lib.file; {
      "composer/composer.json" = {
        source = (mkOutOfStoreSymlink "${composerConfigDir}/composer.json");
      };

      "composer/composer.lock" = {
        source = (mkOutOfStoreSymlink "${composerConfigDir}/composer.lock");
      };
    };
  };
}
