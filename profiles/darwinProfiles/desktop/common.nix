{
  pkgs,
  config,
  ...
}: let
  inherit (config.homebrew) brewPrefix;
in {
  time.timeZone = "America/New_York";

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
    "visual-studio-code"
    "zoom"
  ];

  homebrew.masApps = {
    "DaisyDisk" = 411643860;
    "Drafts" = 1435957248;
    "Magnet" = 441258766;
    "Moom" = 419330170;
    "Paprika Recipe Manager" = 1303222628;
    "Reeder" = 1529448980;
    "Tailscale" = 1475387142;
    "Xcode" = 497799835;
  };
}
