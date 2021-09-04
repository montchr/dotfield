{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.editors.vim;
  configDir = config.dotfield.flkConfigDir;
in {
  options = { my.modules.editors.vim.enable = mkEnableOption true; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ neovim vim ];

    my.hm.configFile."vim" = {
      source = configDir;
      recursive = true;
    };
  };
}
