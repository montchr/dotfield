{
  config,
  lib,
  pkgs,
  system,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
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
      extra-platforms = l.mkIf isAarch64 ["x86_64-darwin" "aarch64-darwin"];
    };
  };

  environment.systemPackages = with pkgs; [
    m-cli
    mas
    # prefmanager
  ];

  environment.variables =
    l.mkIf config.homebrew.enable
    (let
      inherit (config.homebrew) brewPrefix;
    in {
      HOMEBREW_PREFIX = brewPrefix;
      HOMEBREW_CELLAR = "${brewPrefix}/Cellar";
      HOMEBREW_REPOSITORY = brewPrefix;
      MANPATH = "${brewPrefix}/share/man:$MANPATH:";
      INFOPATH = "${brewPrefix}/share/info:$INFOPATH";
    });

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
  services.nix-daemon.enable = true;

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
