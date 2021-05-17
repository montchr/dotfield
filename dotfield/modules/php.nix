{ config, pkgs, ... }:
with lib;
let
  cfg = config.my.modules.editors.emacs;
  configDir = config.dotfield.configDir;
in
{
  options = with lib; {
    my.modules.php = {
      enable = mkEnableOption false;
    };
  };

  config = mkIf cfg.enable {
    my.user.packages = [
      php
      php74packages.composer
    ];
  };
}
