{
  config,
  lib,
  pkgs,
  system,
  self,
  ...
}: let
  l = lib // builtins;
in {
  nix = {
    configureBuildUsers = true;
    # FIXME: needs flake-compat
    # nixPath = mkBefore ["darwin-config=${self}"];
    settings = {
      # Administrative users on Darwin systems are part of the admin group.
      trusted-users = ["@admin"];
      # Required for building some incompatible packages via Rosetta.
      extra-platforms = l.mkIf (system == "aarch64-darwin") ["x86_64-darwin" "aarch64-darwin"];
    };
  };

  environment.systemPackages = with pkgs; [
    #  Swiss Army Knife for macOS
    # => https://github.com/rgcr/m-cli
    m-cli
    mas
    # FIXME: broken on aarch64? error when run: "Bad CPU type in executable"
    # terminal-notifier

    # A tool for managing macOS defaults.
    # https://github.com/malob/prefmanager
    # TODO: re-enable
    # prefmanager
  ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;

  environment.shellInit = l.mkAfter (l.optionalString config.homebrew.enable ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '');

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    # Use the nix-darwin brewfile when invoking `brew bundle` imperatively.
    global.brewfile = true;
  };

  networking.dns = [
    "1.1.1.1"
    "1.0.0.1"
    "2606:4700:4700::1111"
    "2606:4700:4700::1001"
  ];

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
