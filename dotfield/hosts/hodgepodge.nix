{ config, pkgs, lib, inputs, ... }: {
  imports = [ ../modules/darwin ];

  nix = {
    # TODO: garbage collection
    # gc = { user = config.my.username; };

    # Auto upgrade nix package and the daemon service.
    # services.nix-daemon.enable = true;
    # nix.package = pkgs.nix;
    # nix.maxJobs = 4;
    # nix.buildCores = 4;
  };

  my = {
    modules = {
      macos.enable = true;

      # TODO
      # mail = { enable = true; };
      # aerc = { enable = true; };
      # youtube-dl.enable = true;
      # irc.enable = true;
      # rescript.enable = false;
      # clojure.enable = true;
      # newsboat.enable = true;
      gpg.enable = true;
    };
  };

  # TODO
  # homebrew.casks = [
  #   "transmit"
  #   "jdownloader"
  #   "signal"
  # ];

  # Requires to be logged in to the AppStore
  # Cleanup doesn't work automatically if you add/remove to list
  # TODO
  # homebrew.masApps = {
  #   Guidance = 412759995;
  #   NextDNS = 1464122853;
  #   Dato = 1470584107;
  #   "Day One" = 1055511498;
  #   WireGuard = 1451685025;
  #   Tweetbot = 1384080005;
  #   Todoist = 585829637;
  #   Sip = 507257563;
  #   Irvue = 1039633667;
  #   Telegram = 747648890;
  #   Tailscale = 1475387142;
  # };

  networking = { hostName = "HodgePodge"; };
  # TODO: What about the options in https://github.com/LnL7/nix-darwin/blob/073935fb9994ccfaa30b658ace9feda2f8bbafee/modules/system/defaults/smb.nix
  # system.defaults.smb = {
  #   NetBIOSName = ${networking.hostName};
  #   ServerDescription = ${networking.hostName};
  # };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
