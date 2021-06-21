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
              "1password-cli"
              "alfred"
              "appcleaner"
              "bartender"
              "basictex"
              "caprine"
              "corelocationcli"
              "dash"
              "db-browser-for-sqlite"
              "docker"
              "dropbox"
              "fantastical"
              "google-chrome"
              "google-drive"
              "hammerspoon"
              "imageoptim"
              "istat-menus"
              "karabiner-elements"
              "keyboard-maestro"
              # "kap"
              "launchcontrol"
              "libreoffice"
              "marked"
              "muzzle"
              "plexamp"
              "qlcolorcode"
              "qlmarkdown"
              "qlstephen"
              "qlvideo"
              "quicklook-json"
              "quicklookase"
              "signal"
              "sketch"
              "slack"
              "steermouse"
              # "virtualbox"
              "visual-studio-code"
              "vlc"
              "webpquicklook"
              "xquartz"
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
