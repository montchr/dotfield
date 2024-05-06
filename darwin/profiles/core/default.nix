{ lib, ... }:
{
  imports = [
    ./__home-manager.nix
    ./homebrew.nix
    ./nix-config.nix
    ./stty-kludge.nix
    ./system-tools.nix
  ];

  # NOTE: These must be enabled in any recent multi-user Nix installation...
  # yet they remain disabled by default in nix-darwin...
  services.nix-daemon.enable = lib.mkForce true;
  nix.configureBuildUsers = lib.mkForce true;

  # Administrative users on Darwin systems are part of the admin group.
  nix.settings.trusted-users = [ "@admin" ];

  # These UI-enhancement plugins come at an even higher performance cost than
  # completion and do not belong in system configuration at all.
  programs.zsh.enableFzfCompletion = lib.mkForce false;
  programs.zsh.enableFzfGit = lib.mkForce false;
  programs.zsh.enableFzfHistory = lib.mkForce false;
  programs.zsh.enableSyntaxHighlighting = lib.mkForce false;

  environment.systemPath = lib.mkBefore [ "$HOME/.local/bin" ];

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
