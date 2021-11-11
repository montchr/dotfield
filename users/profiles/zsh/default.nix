{ config, lib, pkgs, ... }:

let
  shellCfg = config.my.modules.shell;
  configDir = "${config.dotfield.configDir}/zsh";
in

{
  imports = [
    ../shell
  ];

  my.user.packages = with pkgs; [
    zsh
    zoxide
  ];

  my.env = rec {
    # zsh paths
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
    ZSH_DATA = "$XDG_DATA_HOME/zsh";

    # zgenom paths
    ZGEN_DIR = "$XDG_DATA_HOME/zsh/sources";
    ZGEN_SRC_DIR = "$XDG_DATA_HOME/zsh/zgenom";
  };

  my.hm.programs.starship = {
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = false;
  };

  my.hm.xdg.configFile = {
    "zsh" = {
      source = configDir;
      recursive = true;
      onChange = ''
        # Remove compiled files.
        fd -uu \
          --extension zwc \
          --exec \
            rm '{}'
      '';
    };

    "zsh/.zshenv".text = ''
      # ${config.my.nix_managed}

      ${shellCfg.envInit}

      # Host-local configuration (oftentimes added by other tools)
      [ -f ~/.zshenv ] && source ~/.zshenv
    '';

    "zsh/extra.zshrc".text = ''
      # ${config.my.nix_managed}
      ${shellCfg.rcInit}
    '';
  };
}
