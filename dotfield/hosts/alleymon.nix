{ config, pkgs, lib, inputs, ... }: {
  imports = [ ../modules/darwin ];

  my = {
    username = "montchr";
    email = "chris@alley.co";
    website = "https://alley.co/";
    modules = {
      macos.enable = true;
      gpg.enable = true;

      # mail = {
      #   enable = true;
      #   account = "Work";
      #   alias_path = "";
      #   keychain = { name = "gmail.com"; };
      #   imap_server = "imap.gmail.com";
      #   smtp_server = "smtp.gmail.com";
      # };
    };
  };

  networking = { hostName = "alleymon"; };
  # TODO: What about the options in https://github.com/LnL7/nix-darwin/blob/073935fb9994ccfaa30b658ace9feda2f8bbafee/modules/system/defaults/smb.nix
  # system.defaults.smb = {
  #   NetBIOSName = ${networking.hostName};
  #   ServerDescription = ${networking.hostName};
  # };

  # nix = {
  #   # TODO garbage collection
  #   # gc = { user = config.my.username; };

  #   # Auto upgrade nix package and the daemon service.
  #   # services.nix-daemon.enable = true;
  #   # nix.package = pkgs.nix;
  #   # nix.maxJobs = 4;
  #   # nix.buildCores = 4;
  # };

  # TODO
  # my.user = {
  #   packages = with pkgs; [
  #     emacs
  #   ];
  # };

  # TODO
  # homebrew.casks = [
  #   "adoptopenjdk8"
  #   "corretto"
  #   "firefox"
  #   "loom"
  #   "ngrok"
  #   "obs"
  #   "obs-virtualcam"
  #   "vagrant"
  #   "ngrok"
  #   "docker"
  # ];

  # Requires to be logged in to the AppStore
  # Cleanup doesn't work automatically if you add/remove to list
  # TODO
  # homebrew.masApps = {
  #   Twitter = 1482454543;
  #   Sip = 507257563;
  #   Xcode = 497799835;
  #   Guidance = 412759995;
  #   Dato = 1470584107;
  #   WireGuard = 1451685025;
  # };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
