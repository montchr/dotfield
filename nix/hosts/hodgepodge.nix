{ config, pkgs, lib, inputs, ... }: {
  imports = [ ../modules/darwin ];

  nix = {
    gc = { user = config.my.username; };
    # Auto upgrade nix package and the daemon service.
    # services.nix-daemon.enable = true;
    # nix.package = pkgs.nix;
    # nix.maxJobs = 4;
    # nix.buildCores = 4;
  };

  my = {
    modules = {
      # TODO: this should be automatic
      macos.enable = true;

      gui.enable = true;
      youtube-dl.enable = true;
    };
  };

  homebrew.casks = [
    "transmit"
    "signal"
  ];

  # Requires to be logged in to the AppStore
  # Cleanup doesn't work automatically if you add/remove to list
  # TODO: edit these
  homebrew.masApps = {
  #   Guidance = 412759995;
  #   NextDNS = 1464122853;
  #   Dato = 1470584107;
  #   "Day One" = 1055511498;
  #   WireGuard = 1451685025;
  #   Tweetbot = 1384080005;
  #   Todoist = 585829637;
    Sip = 507257563;
  #   Irvue = 1039633667;
  #   Telegram = 747648890;
  #   Tailscale = 1475387142;
  };

  networking = { hostName = "HodgePodge"; };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # TODO: added by CDOM
  # home.stateVersion = "21.05";
}
