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
  my.hm.home.file.".zshenv".text = (if pkgs.stdenv.isDarwin then ''
    export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
    export ZSH_CACHE="$XDG_CACHE_HOME/zsh"
    export ZSH_DATA="$XDG_DATA_HOME/zsh"
    export HISTFILE="$XDG_STATE_HOME/zsh/history"

    ${builtins.readFile ./z4h-env.hodge.zsh}
    ${shellCfg.envInit}
  '' else "");

  my.hm.xdg.configFile = lib.mkMerge [
    {
      "zsh/.zshrc".source = "${configDir}/main.hodge.zsh";
      "zsh/config.zsh".source = "${configDir}/config.zsh";
      "zsh/functions.zsh".source = "${configDir}/functions.zsh";
      "zsh/extra.zshrc".text = ''
        ${shellCfg.rcInit}
      '';
    }
  ];
}
