{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./hammerspoon.nix
    ./yabai.nix
  ];

  environment.systemPackages = with pkgs; [
    gtk-mac-integration
  ];

  environment.variables = {
    TERMINFO_DIRS = "/Applications/kitty.app/Contents/Resources/kitty/terminfo";
  };

  homebrew.taps = [
    "FelixKratz/formulae"
    "homebrew/cask"
    "homebrew/cask-versions"
  ];

  homebrew.brews = [
    # This provides a GUI, despite it not being a cask.
    "pinentry-mac"
  ];

  homebrew.casks = [
    "1password-cli"
    "adobe-acrobat-reader"
    "alfred"
    "appcleaner"
    "bartender"
    "bitwarden"
    "brave-browser"
    "basictex"
    "calibre"
    "caprine"
    "corelocationcli"
    "docker"
    "dropbox"
    "eloston-chromium"
    "fantastical"
    "firefox-developer-edition"
    "flameshot"
    "google-chrome"
    "google-drive"
    "imageoptim"
    "istat-menus"
    "kap"
    "karabiner-elements"
    "keyboard-maestro"
    "kitty"
    # "libreoffice"
    "marked"
    "mpv"
    "nextcloud"
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
    "soundsource"
    "spotify"
    "steermouse"
    "sublime-text"
    "transmit"
    "vagrant"
    "vimr"
    # Disabled because updates to VirtualBox are disruptive
    # "virtualbox"
    "visual-studio-code"
    "webpquicklook"
    # "zoom"
  ];

  # Disabled because these greatly slow down installation time.
  # homebrew.masApps = {
  #   "Affinity Photo" = 824183456;
  #   "Be Focused Pro" = 961632517;
  #   "Canary Mail" = 1236045954;
  #   "DaisyDisk" = 411643860;
  #   "Deliveries" = 924726344;
  #   "Drafts" = 1435957248;
  #   "GoodTask" = 1143437985;
  #   "Keka" = 470158793;
  #   "NepTunes" = 1006739057;
  #   "New File Menu" = 1064959555;
  #   "Reeder" = 1529448980;
  #   "Tailscale" = 1475387142;
  # };
}
