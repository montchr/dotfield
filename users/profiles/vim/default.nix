{ config, lib, pkgs, ... }:

{
  my.env = {
    SPACEVIMDIR = "$XDG_CONFIG_HOME/spacevim";
  };

  my.hm.programs.neovim = {
    enable = true;
    vimAlias = true;
    withNodeJs = true;
    withPython3 = true;
  };

}
