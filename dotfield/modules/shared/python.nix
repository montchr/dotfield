{ pkgs, lib, config, ... }:

let

  cfg = config.my.modules.python;
  # dotfield = config.dotfield;

in {
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
        # env = { PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/config.py"; };

        user = {
          packages = with pkgs;
            [
              # TODO
              (python3.withPackages
                (ps: with ps; [ pip black setuptools pylint grip ]))
            ];
        };

        # hm.configFile = {
        #   "python" = {
        #     recursive = true;
        #     source = builtins.toPath /. "${dotfield.configDir}/python";
        #   };
        #   "pip" = {
        #     recursive = true;
        #     source = builtins.toPath /. "${dotfield.configDir}/pip";
        #   };
        # };
      };
    };
}
