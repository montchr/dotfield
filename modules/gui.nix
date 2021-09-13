{ pkgs, lib, config, options, ... }:

let cfg = config.my.modules.gui;
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
    mkIf cfg.enable (mkMerge [
      (if (builtins.hasAttr "homebrew" options) then {
        homebrew = {
          taps = [ "homebrew/cask" "homebrew/cask-versions" ];

          casks = [
            "1password"
            "1password-cli"
            "alfred"
            "appcleaner"
            "bartender"
            "basictex"
            "caprine"
            "corelocationcli"
            "db-browser-for-sqlite"
            "docker"
            "dropbox"
            "fantastical"
            "firefox-developer-edition"
            "google-chrome"
            "google-drive"
            (optionalString config.my.modules.hammerspoon.enable "hammerspoon")
            "imageoptim"
            "istat-menus"
            "kap"
            "karabiner-elements"
            "keyboard-maestro"
            (optionalString config.my.modules.kitty.enable "kitty")
            "libreoffice"
            "marked"
            "pdf-expert"
            "plexamp"
            "qlcolorcode"
            "qlmarkdown"
            "qlstephen"
            "qlvideo"
            "quicklook-json"
            "quicklookase"
            "signal"
            "slack"
            "spotify"
            "steermouse"
            "sublime-text"
            "ubersicht"
            "vagrant"
            # Disabled because updates to VirtualBox are disruptive
            # "virtualbox"
            "visual-studio-code"
            "vlc"
            "webpquicklook"
            # TODO: Why is xquartz necessary?
            "xquartz"
            "zoom"
          ];

          masApps = {
            "Affinity Photo" = 824183456;
            "Be Focused Pro" = 961632517;
            "Canary Mail" = 1236045954;
            "DaisyDisk" = 411643860;
            "Deliveries" = 924726344;
            "Drafts" = 1435957248;
            "GoodTask" = 1143437985;
            "Keka" = 470158793;
            "NepTunes" = 1006739057;
            "New File Menu" = 1064959555;
            "Reeder 5." = 1529448980;
          };
        };
      } else {
        my.user = { packages = with pkgs; [ signal-desktop ]; };
      })
    ]);
}
