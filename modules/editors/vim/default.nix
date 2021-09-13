{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.editors.vim;
  configDir = config.dotfield.configDir;
in
{
  options = { my.modules.editors.vim.enable = mkEnableOption true; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ vim ];

    my.hm.configFile."vim" = {
      source = configDir;
      recursive = true;
    };
  };
}
