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

  # These essential environment variables must be set in a place zsh will always
  # look due to the bootstrap problem.
  # https://wiki.archlinux.org/title/XDG_Base_Directory
  my.hm.home.file.".zshenv".text = ''
    export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
    export ZSH_CACHE="$XDG_CACHE_HOME/zsh"
    export ZSH_DATA="$XDG_DATA_HOME/zsh"
    export HISTFILE="$XDG_STATE_HOME/zsh/history"
  '';

  my.hm.xdg.configFile = {
    "zsh/.zshenv".text = ''
      ${builtins.readFile ./z4h-env.zsh}
      ${shellCfg.envInit}
    '';

    "zsh/.zshrc".source = mkOutOfStoreSymlink "${configDirPath}/main.zsh";

    "zsh/config.zsh".source = mkOutOfStoreSymlink "${configDirPath}/config.zsh";
    "zsh/functions.zsh".source = mkOutOfStoreSymlink "${configDirPath}/functions.zsh";

    "zsh/extra.zshrc".text = ''
      ${shellCfg.rcInit}
    '';
  };
}
