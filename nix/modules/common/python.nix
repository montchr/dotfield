{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.python;
in
{
  options = with lib; {
    my.modules.python = {
      enable = mkEnableOption ''
        Whether to enable python module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my = {
        env = {
          PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/config.py";
        };

        user = {
          packages = with pkgs;
            [
              (python3.withPackages (ps:
                with ps; [
                  pip
                  setuptools

                  black
                  pylint
                  grip
                ]))
            ];
        };

        hm.file = {
          ".config/python" = {
            recursive = true;
            source = ../../../config/python;
          };
          ".config/pip" = {
            recursive = true;
            source = ../../../config/pip;
          };
        };
      };
    };
}
