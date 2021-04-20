{ pkgs, lib, config, options, ... }:

let
  cfg = config.my.modules.gui;
in
{
  options = with lib; {
    my.modules.gui = {
      enable = mkEnableOption ''
        Whether to enable gui module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.user = {
        packages = with pkgs; [
          brave
          firefox
          obsidian
          zoom-us
          signal-desktop
          vscodium
          slack
          # sqlitebrowser
          # virtualbox
        ];
      };
    }
}
