{
  config,
  lib,
  pkgs,
  system,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
  l = lib // builtins;
  inherit (config.homebrew) brewPrefix;
in {
  imports = [./builders/nixbuild-net.nix];

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

  # These UI-enhancement plugins come at an even higher performance cost than
  # completion and do not belong in system configuration at all.
  programs.zsh.enableFzfCompletion = l.mkForce false;
  programs.zsh.enableFzfGit = l.mkForce false;
  programs.zsh.enableFzfHistory = l.mkForce false;
  programs.zsh.enableSyntaxHighlighting = l.mkForce false;

  environment.systemPackages = with pkgs; [
    m-cli
    mas
    # prefmanager
  ];

  environment.variables = {
    HOMEBREW_PREFIX = brewPrefix;
    HOMEBREW_CELLAR = "${brewPrefix}/Cellar";
    HOMEBREW_REPOSITORY = brewPrefix;
    INFOPATH = "${brewPrefix}/share/info:$INFOPATH";
    MANPATH = "${brewPrefix}/share/man:$MANPATH:";
  };

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
