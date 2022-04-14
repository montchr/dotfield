{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: let
  inherit (config) my;
in {
  imports = with suites;
    gui
    ++ personal
    ++ work;

  home-manager.users.montchr = {
    config,
    suites,
    ...
  }: {
    imports = with suites; [hmUsers.xtallos] ++ darwin ++ gui;

    accounts.email.accounts.work.primary = true;
    accounts.email.accounts.personal.primary = false;

    home.packages = with pkgs; [ngrok];

    programs.firefox.profiles = {
      home.isDefault = false;
      work.isDefault = true;
    };

    programs.git.includes = [
      {
        condition = "gitdir:~/broadway/**";
        contents = {
          user.email = pkgs.lib.our.whoami.emails.work;
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

  my = {
    username = "montchr";
    website = "https://alley.co/";
  };

  environment.variables = {
    PATH = ["$HOME/broadway/bin" "$PATH"];
  };

  homebrew = {
    casks = ["figma" "microsoft-teams" "sketch"];
    masApps = {
      "Harvest" = 506189836;
      "Jira" = 1475897096;
      "xScope" = 889428659;
      "Xcode" = 497799835;
    };
  };
}
