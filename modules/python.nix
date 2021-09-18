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
        user = {
          packages = with pkgs;
            [
              (python3.withPackages
                (ps: with ps; [
                  pip
                  black
                  setuptools
                  pylint
                  grip
                ]))
            ];
        };
      };
    };
}
