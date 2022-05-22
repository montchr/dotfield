{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports =
    (with suites; workstation)
    ++ (with profiles; [
      profiles.virtualisation.virtualbox
    ]);

  # N.B. All of these have unusable defaults and must be set manually.
  users.users.montchr.home = "/Users/montchr";
  users.users.montchr.isHidden = false;
  users.users.montchr.shell = pkgs.zsh;

  home-manager.users.montchr = hmArgs: {
    imports =
      [hmUsers.xtallos]
      ++ (with hmArgs.suites; workstation)
      ++ (with hmArgs.profiles; [
        aws
        mail
        php
        ruby
        virtualisation.vagrant
      ]);

    accounts.email.accounts.work.primary = true;
    accounts.email.accounts.personal.primary = false;

    home.packages = with pkgs; [
      ngrok
      # TODO: move this to a common profile when pandas dep is fixed upstream
      visidata # A terminal spreadsheet multitool for discovering and arranging data
    ];

    programs.firefox.profiles = {
      home.isDefault = false;
      work.isDefault = true;
    };

    programs.git.includes = [
      {
        condition = "gitdir:~/broadway/**";
        contents = {
          user.email = hmArgs.config.accounts.email.accounts.work.userName;
        };
      }
    ];
  };

  networking.hostName = "alleymon";

  # MacBookPro16,2
  nixpkgs.system = "x86_64-darwin";
  # 2.3 GHz Quad-Core Intel Core i7
  nix.maxJobs = 4;
  nix.buildCores = 0;
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Bluetooth PAN"
    "Thunderbolt Bridge"
    "USB 10/100/1000 LAN"
    "Wi-Fi"
  ];

  environment.variables = {
    PATH = ["$HOME/broadway/bin" "$PATH"];
  };

  homebrew = {
    casks = ["figma" "microsoft-teams" "sketch"];
    # Disabled because these greatly slow down installation time.
    # masApps = {
    #   "Harvest" = 506189836;
    #   "Jira" = 1475897096;
    #   "xScope" = 889428659;
    #   # Xcode should be installed manually since any update will take a very
    #   # long time to download and install.
    #   # "Xcode" = 497799835;
    # };
  };
}
