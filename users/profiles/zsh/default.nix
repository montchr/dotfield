{ config, lib, pkgs, ... }:

let
  inherit (config) my;
  inherit (config.home-manager.users.${my.username}.lib.file) mkOutOfStoreSymlink;

  shellCfg = config.shell;
  configDir = "${config.dotfield.configDir}/zsh";
  configDirPath = "${config.dotfield.path}/config/zsh";
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
  };

  my.hm.xdg.configFile = {
    "zsh" = {
      source = configDir;
      # FIXME: conflicts with in-store symlinks in same target directory causing CI workflows to fail
      # source = mkOutOfStoreSymlink configDirPath;
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
      ${shellCfg.envInit}

      # Host-local configuration (oftentimes added by other tools)
      [ -f ~/.zshenv ] && source ~/.zshenv
    '';

    "zsh/extra.zshrc".text = ''
      ${shellCfg.rcInit}
    '';
  };
}
