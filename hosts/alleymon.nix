{ config, pkgs, ... }: {
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

    modules = {
      php.enable = true;
    };

    hm.accounts.email.accounts.work.primary = true;
  };

  environment.variables = {
    PATH = [ "$HOME/broadway/bin" "$PATH" ];
  };

  environment.systemPackages = with pkgs; [ dnsmasq ];

  homebrew = {
    casks = [ "figma" "microsoft-teams" "sketch" ];
    masApps = {
      "Harvest" = 506189836;
      "Jira" = 1475897096;
      "xScope" = 889428659;
      "Xcode" = 497799835;
    };
  };

  services.dnsmasq = {
    enable = false;
    addresses = {
      # Vagrant boxes.
      http = "192.168.50.4";
      test = "192.168.50.4";
    };
  };
}
