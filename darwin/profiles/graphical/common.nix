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

  # kitty terminfo must be applied on the system level
  # https://github.com/nix-community/home-manager/issues/423
  environment.variables = {
    TERMINFO_DIRS = ["${pkgs.kitty.terminfo.outPath}/share/terminfo"];
  };

  homebrew.taps = [
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
    "alfred"
    "appcleaner"
    "bartender"
    "calibre"
    "eloston-chromium" #          <- aka "ungoogled-chromium" in nixpkgs
    "dropbox"
    "fantastical"
    "firefox" #                   <- "home" browser
    "firefox-developer-edition" # <- "work" browser
    "flameshot"
    "karabiner-elements"
    "mpv"
    "plexamp"
    "signal"
    "soundsource"
    "spotify"
    "steermouse" # <- for input device mapping
    "vscodium"
    "zoom"
  ];

  homebrew.masApps = {
    "DaisyDisk" = 411643860;
    "Drafts" = 1435957248;
    "Paprika Recipe Manager" = 1303222628;
    "Reeder" = 1529448980;
    # "Tailscale" = 1475387142;
  };
}
