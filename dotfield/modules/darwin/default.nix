{ config, inputs, lib, pkgs, ... }:
{
  nix = {
    trustedUsers = [ "@admin" ];

    # Auto upgrade nix package and the daemon service.
    package = pkgs.nix;
    maxJobs = 4;
    buildCores = 4;
  };

  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;

  my = {
    modules = {
      macos.enable = true;
      gpg.enable = true;
    };
  };

  homebrew = {
    enable = true;
    autoUpdate = true;
    # TODO
    # cleanup = "zap";
    # global.brewfile = true;
    global.noLock = true;
  };

  imports = [ ./macos.nix ];

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
