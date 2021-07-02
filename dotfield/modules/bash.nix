{ pkgs, lib, config, inputs, options, ... }:

with lib;
let
  configDir = config.dotfield.configDir;
  home = config.my.user.home;
  cfg = config.my.modules.bash;
in {
  options = with lib; {
    my.modules.bash = {
      enable = mkEnableOption ''
        Whether to enable bash module
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      enableCompletion = true;
      interactiveShellInit = "source $HOME/.config/bash/bashrc";
    };

    my = {
      env = { BASH_COMPLETION_USER_FILE = "$XDG_DATA_HOME/bash/completion"; };

      hm = {
        configFile = {

          "bash/bashrc".text = ''
            # ${config.my.nix_managed}

            ${builtins.readFile "${configDir}/shell/profile"}
          '';

        };
      };
    };

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment = {
      shells = with pkgs; [ bashInteractive_5 ];

      systemPackages = with pkgs; [ bashInteractive_5 ];
    };
  };
}
