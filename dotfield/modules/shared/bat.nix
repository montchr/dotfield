{ pkgs, lib, config, ... }:

let

  cfg = config.my.modules.bat;

in {
  options = with lib; {
    my.modules.bat = {
      enable = mkEnableOption ''
        Whether to enable bat module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.env = { BAT_CONFIG_PATH = "$XDG_CONFIG_HOME/bat/config"; };

      my.user = { packages = with pkgs; [ bat ]; };

      my.hm.configFile = {
        "bat/batrc" = {
          text = ''
            --theme="base16-256"
            --style="plain,changes"
            --italic-text=always
            --map-syntax '.*ignore:Git Ignore'
            --map-syntax '.gitconfig.local:Git Config'
            --map-syntax '**/mx*:Bourne Again Shell (bash)'
            --map-syntax '**/completions/_*:Bourne Again Shell (bash)'
            --map-syntax '.zsh*:Bourne Again Shell (bash)'
            --map-syntax '.vimrc.local:VimL'
            --map-syntax 'vimrc:VimL'
          '';
        };
      };
    };
}
