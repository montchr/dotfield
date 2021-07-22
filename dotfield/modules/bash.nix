{ pkgs, lib, config, inputs, options, ... }:

with lib;
let
  configDir = config.dotfield.configDir;
  home = config.my.user.home;
  cfg = config.my.modules.bash;
  bash = pkgs.bashInteractive_5;
  profileInit = "${configDir}/shell/profile";
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

      user.packages = [ bash ];

      hm = {
        configFile = {
          "bash/bashrc".text = ''
            # ${config.my.nix_managed}

            ${builtins.readFile profileInit}
          '';

        };
      };
    };

    environment = {
      shells = [ bash ];
      systemPackages = [ bash ];
      variables = {
        BASH_ENV = profileInit;
      };
    };
  };
}
