{ config, lib, pkgs, ... }:

{
  nixpkgs.system = "x86_64-darwin";

  # Administrative users on Darwin are part of this group, not the `wheel` group.
  nix.trustedUsers = [ "@admin" ];

  environment.systemPackages = with pkgs; [
    mas
    terminal-notifier
  ];

  homebrew = {
    # enable = true;
    enable = false;
    autoUpdate = false;
    global.noLock = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
