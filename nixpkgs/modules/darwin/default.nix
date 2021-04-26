{ config, pkgs, ... }:

{
  nix = {
    trustedUsers = [
      "@admin"
    ];
  };

  services = {
    nix-daemon.enable = true;
  };

  users.nix.configureBuildUsers = true;

  homebrew.enable = true;
  homebrew.autoUpdate = true;
  # TODO
  # homebrew.cleanup = "zap";
  # homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  imports = [ ./macos.nix ];
}
