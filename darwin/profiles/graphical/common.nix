{
  config,
  lib,
  pkgs,
  ...
}: {
  security.pam.enableSudoTouchIdAuth = true;

  environment.systemPackages = with pkgs; [
    gtk-mac-integration
  ];

  homebrew.taps = [
    # TODO: which cask is this for? sketchybar iirc?
    # "FelixKratz/formulae"
    "homebrew/cask"
    "homebrew/cask-versions"
  ];

  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Wi-Fi"
    "Thunderbolt Bridge"
  ];

  homebrew.brews = [
    # This provides a GUI, despite it not being a cask.
    # TODO: are there any better alternatives yet???
    # "pinentry-mac"
  ];

  homebrew.casks = [
    # TODO: use creative cloud?
    # "adobe-acrobat-reader"
    "alfred"
    "appcleaner"
    "bartender"
    "calibre"
    "eloston-chromium" # aka "ungoogled-chromium" in nixpkgs
    "fantastical"
    "firefox"
    "firefox-developer-edition"
    "flameshot"
    # "istat-menus" # TODO: replace w/some cli tools or scripty things for a bar?
    "kap" # screen recorder... written w/next.js?!
    # TODO: there might be a nix-darwin module for karabiner-elements soon
    "karabiner-elements"
    "mpv"
    # "nextcloud"
    "plexamp"
    "qlmarkdown"
    "quicklook-json"
    "quicklookase"
    "signal"
    "soundsource"
    "spotify"
    "steermouse" # for input device mapping
    "vscodium"
    "webpquicklook"
    "zoom"
  ];

  # Disabled because these greatly slow down installation time.
  homebrew.masApps = {
    "DaisyDisk" = 411643860;
    # "Deliveries" = 924726344;
    # "Drafts" = 1435957248;
    # "GoodTask" = 1143437985;
    # "Keka" = 470158793;
    # "NepTunes" = 1006739057;
    # "New File Menu" = 1064959555;
    # "Reeder" = 1529448980;
    # "Tailscale" = 1475387142;
  };
}
