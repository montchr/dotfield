{ config, lib, pkgs, inputs, ... }:

# TODO: espanso doesn't install correctly on darwin! "unsupported system"
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

      # TODO: results in error
      ${builtins.map (p: "espanso package install ${p}") plugins}
    '';
  };
}
