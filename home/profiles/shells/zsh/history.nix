{ config, ... }:
let
  ZSH_DATA = "${config.xdg.dataHome}/zsh";
in
{
  programs.zsh = {
    history.path = "${ZSH_DATA}/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;
    history.ignoreSpace = true;
    history.save = 10000;
    history.share = true;
    history.size = 10000;
  };
}
