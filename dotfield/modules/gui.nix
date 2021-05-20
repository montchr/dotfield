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
    mkIf cfg.enable (
      mkMerge [
        (
          if (builtins.hasAttr "homebrew" options) then {
            homebrew.taps = [ "homebrew/cask" "homebrew/cask-versions" ];
            homebrew.casks = [
              "1password"
              "alfred"
              "appcleaner"
              "corelocationcli"
              "db-browser-for-sqlite"
              "google-chrome"
              "google-drive"
              "hammerspoon"
              "imageoptim"
              # "kap"
              "launchcontrol"
              "sketch"
              "slack"
              # "virtualbox"
              "visual-studio-code"
              "zoom"
            ];

            # my.hm.file = {
            #   ".hammerspoon" = {
            #     recursive = true;
            #     source = ../../../config/.hammerspoon;
            #   };
            # };
          } else {
            my.user = {
              packages = with pkgs; [ firefox zoom-us signal-desktop slack ];
            };
          }
        )
      ]
    );
}
