{ config, pkgs, ... }:

{
  # TODO: at the very least, this may not be necessary with a single-user
  # installation. at most, it could be destructive...
  # users.nix.configureBuildUsers = true;

  homebrew.enable = true;
  homebrew.autoUpdate = true;
  homebrew.cleanup = "zap";
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  imports = [
    ./macos.nix
    ./gui.nix
  ];
}
