{ pkgs, lib, config, options, ... }:

let

  cfg = config.my.modules.gui;

in {
  options = with lib; {
    my.modules.gui = {
      enable = mkEnableOption ''
        Whether to enable gui module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable (mkMerge [
      (if (builtins.hasAttr "homebrew" options) then {
        # [todo] (automate) Requires homebrew to be installed
        homebrew.taps = [ "homebrew/cask" "homebrew/cask-versions" ];
        homebrew.casks = [
          "1password"
          "alfred"
          "appcleaner"
          "corelocationcli"
          "db-browser-for-sqlite"
          "figma"
          "google-chrome"
          "hammerspoon"
          "imageoptim"
          # "kap"
          "launchcontrol"
          "slack"
          # "sync"
          # "virtualbox"
          "visual-studio-code"
          "zoom"
        ];

        my.hm.file = {
          ".hammerspoon" = {
            recursive = true;
            source = ../../../config/.hammerspoon;
          };
        };
      } else {
        my.user = {
          packages = with pkgs; [ firefox zoom-us signal-desktop slack ];
        };
      })
    ]);
}
