{ pkgs, lib, config, options, ... }:

let
  cfg = config.my.modules.gui;
in
{
  options = with lib; {
    my.modules.gui = {
      enable = mkEnableOption ''
        Whether to enable gui module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      homebrew.taps = [ "homebrew/cask" "homebrew/cask-versions" ];
      homebrew.brews = [ "mas" ];
      homebrew.casks = [
        "1password"
        "alfred"
        "appcleaner"
        "brave-browser"
        # "corelocationcli"
        "figma"
        "google-chrome"
        "hammerspoon"
        # "imageoptim"
        # "kap"
        # "launchcontrol"
        "slack"
        # "sync"
        # "virtualbox"
        "zoom"
        # "vscodium"
      ];

      my.hm.file = {
        ".hammerspoon" = {
          recursive = true;
          # source = ../../../config/.hammerspoon;
          source = ../../../config/hammerspoon;
        };
      };
    };
}
