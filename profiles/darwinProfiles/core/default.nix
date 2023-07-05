{
  flake,
  config,
  pkgs,
  ...
}: let
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./homebrew.nix
    ./nix-optimizations-darwin.nix
  ];

  # These should (must?) be enabled in any recent multi-user Nix installation,
  # and yet they remain disabled by default in nix-darwin...
  services.nix-daemon.enable = l.mkForce true;
  nix.configureBuildUsers = l.mkForce true;

  # Administrative users on Darwin systems are part of the admin group.
  nix.settings.trusted-users = ["@admin"];

  nix.distributedBuilds = l.mkDefault true;

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

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
