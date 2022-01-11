{ config, pkgs, suites, ... }:

{
  imports = with suites;
    darwin-gui
    ++ personal
    ++ developer
    ++ work;

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
    email = "chris@alley.co";
    website = "https://alley.co/";

    hm.accounts.email.accounts.work.primary = true;

    hm.programs.firefox.profiles = {
      home.isDefault = false;
      work.isDefault = true;
    };
  };

  environment.variables = {
    PATH = [ "$HOME/broadway/bin" "$PATH" ];
  };

  homebrew = {
    casks = [ "figma" "microsoft-teams" "sketch" ];
    masApps = {
      "Harvest" = 506189836;
      "Jira" = 1475897096;
      "xScope" = 889428659;
      "Xcode" = 497799835;
    };
  };
}
