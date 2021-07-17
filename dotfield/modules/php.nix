{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.my.modules.php;
in {
  options = { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    my.user.packages = with pkgs; [ php php74Packages.composer ];
  };
}
