{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib) dotfield;
in {
  # Administrative users on Darwin systems are part of the admin group by default.
  nix.trustedUsers = ["@admin" "@wheel"];

  environment.systemPackages = with pkgs; [
    #  Swiss Army Knife for macOS
    # => https://github.com/rgcr/m-cli
    m-cli
    mas
    terminal-notifier

    # A tool for managing macOS defaults.
    # https://github.com/malob/prefmanager
    # FIXME: `prefmanager` build fails with sandbox mode enabled
    # https://github.com/malob/prefmanager/issues/2
    # prefmanager
  ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;

  homebrew = {
    enable = true;
    # use the nix-darwin brewfile when invoking `brew bundle` imperatively
    global.brewfile = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
