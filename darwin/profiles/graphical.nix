{
  pkgs,
  config,
  ...
}: let
  inherit (config.homebrew) brewPrefix;
in {
  security.pam.enableSudoTouchIdAuth = true;

  # kitty terminfo must be applied on the system level
  # https://github.com/nix-community/home-manager/issues/423
  environment.variables = {
    TERMINFO_DIRS = ["${pkgs.kitty.terminfo.outPath}/share/terminfo"];
  };

  # Allow for usage of `brew` CLI without adding to `PATH`
  environment.shellAliases."brew" = "${brewPrefix}/brew";

  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-versions"
  ];

  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Wi-Fi"
    "Thunderbolt Bridge"
  ];

  homebrew.casks = [
    "airfoil"
    "alfred"
    "appcleaner"
    "bartender"
    "calibre"
    "caprine"
    "eloston-chromium" #          <- aka "ungoogled-chromium" in nixpkgs
    "dropbox"
    "fantastical"
    "firefox" #                   <- "home" browser
    "firefox-developer-edition" # <- "work" browser
    "flameshot"
    "karabiner-elements"
    "keycastr"
    "obs"
    "plexamp"
    "raindropio"
    "signal"
    "soundsource"
    "spotify"
    "steermouse" # <- for input device mapping
    "vlc"
    "vscodium"
    "zoom"
  ];

  homebrew.masApps = {
    "DaisyDisk" = 411643860;
    "Drafts" = 1435957248;
    "Paprika Recipe Manager" = 1303222628;
    "Reeder" = 1529448980;
    "Tailscale" = 1475387142;
    "Xcode" = 497799835;
  };
}
