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
      users.chrismont
      # profiles.virtualisation.virtualbox
    ]);

  home-manager.users.montchr = hmArgs: {
    imports =
      [hmUsers.cdom]
      ++ (with hmArgs.suites; developer ++ workstation)
      ++ (with hmArgs.profiles; [
        aws
        nodejs
        php
      ]);

    home.packages = with pkgs; [];

    programs.firefox.profiles = {
      home.isDefault = false;
      work.isDefault = true;
    };
  };

  # FIXME: determine a final hostname for the loaner
  networking.hostName = "tmpln";

  # FIXME: update these to match correct specs
  # MacBookPro16,2
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
