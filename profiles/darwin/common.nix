{ config, lib, pkgs, ... }:

{
  nixpkgs.system = "x86_64-darwin";

  # Administrative users on Darwin are part of this group, not the `wheel` group.
  nix.trustedUsers = [ "@admin" ];

  environment.systemPackages = with pkgs; [
    mas
    terminal-notifier
  ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;

  homebrew = {
    enable = true;
    # enable = false;
    autoUpdate = true;
    global.noLock = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
