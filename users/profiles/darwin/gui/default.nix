{ config, lib, pkgs, ... }:
let
  configDir = "${config.dotfield.configDir}/darwin";
in
{
  imports = [
    ./hammerspoon.nix
    ./sketchybar.nix
    ./yabai.nix
  ];

  my.user.packages = with pkgs; [
    (writeScriptBin "toggle-dark-mode"
      (builtins.readFile "${configDir}/bin/toggle-dark-mode"))
  ];

  homebrew.taps = [
    "FelixKratz/formulae"
    "homebrew/cask"
    "homebrew/cask-versions"
  ];

  homebrew.casks = [
    "1password"
    "1password-cli"
    "alfred"
    "appcleaner"
    "bartender"
    "basictex"
    "caprine"
    "corelocationcli"
    "docker"
    "dropbox"
    "fantastical"
    "firefox-developer-edition"
    "google-chrome"
    "google-drive"
    "imageoptim"
    "istat-menus"
    "kap"
    "karabiner-elements"
    "keyboard-maestro"
    # "kitty"
    # "libreoffice"
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
    # "xquartz"
    # "zoom"
  ];

  homebrew.masApps = {
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

}
