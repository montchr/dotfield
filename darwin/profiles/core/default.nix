{
  inputs,
  config,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
  l = inputs.nixpkgs.lib // builtins;

  # NOTE: This is NOT the same as upstream's `homebrew.brewPrefix` option,
  # which defaults to the equivalent of `$(brew --prefix)/bin`.
  # <https://github.com/LnL7/nix-darwin/issues/596>
  brewPrefix =
    if isAarch64
    then "/opt/homebrew"
    else "/usr/local";
in {
  imports = [./builders/nixbuild-net.nix];

  # These should (must?) be enabled in any recent multi-user Nix installation,
  # and yet they remain disabled by default in nix-darwin...
  services.nix-daemon.enable = l.mkForce true;
  nix.configureBuildUsers = l.mkForce true;

  # Administrative users on Darwin systems are part of the admin group.
  nix.settings.trusted-users = ["@admin"];

  nix.distributedBuilds = l.mkDefault true;

  # FIXME: currently requires running `nix run nixpkgs#darwin.builder`
  # manually in a separate shell session
  nix.nixos-builder-vm.enable = true;

  # FIXME: needs flake-compat
  # nix.nixPath = mkBefore ["darwin-config=${self}"];

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

  # <https://github.com/LnL7/nix-darwin/issues/596>
  #
  # $ brew shellenv
  # export HOMEBREW_PREFIX="/opt/homebrew";
  # export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
  # export HOMEBREW_REPOSITORY="/opt/homebrew";
  # export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
  # export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
  # export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
  environment.systemPath = l.mkBefore ["${brewPrefix}/bin" "${brewPrefix}/sbin"];
  environment.variables = {
    HOMEBREW_PREFIX = brewPrefix;
    HOMEBREW_CELLAR = "${brewPrefix}/Cellar";
    HOMEBREW_REPOSITORY = brewPrefix;
    INFOPATH = "${brewPrefix}/share/info:\${INFOPATH:-}";
    MANPATH = "${brewPrefix}/share/man\${MANPATH+:$MANPATH}:";
  };

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
