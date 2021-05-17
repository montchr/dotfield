{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.espanso;
  configDir = config.dotfield.configDir;
  plugins = [ "greek-letters-alt" ];
in
{
  options = with lib; {
    my.modules.espanso = {
      enable = mkEnableOption false;
    };
  };

  config = mkIf cfg.enable {
    my = {
      user.packages = with pkgs; [
        espanso
      ];

      hm = {
        configFile = {
          "espanso" = {
            source = "${configDir}/espanso";
            recursive = true;
          };
        };
      };
    };

    system.activationScripts.postUserActivation.text = ''
      espanso register
    '';
  };
}
