{
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./homebrew.nix
    ./nix-optimizations-darwin.nix
  ];

  # These should (must?) be enabled in any recent multi-user Nix installation,
  # and yet they remain disabled by default in nix-darwin...
  services.nix-daemon.enable = lib.mkForce true;
  nix.configureBuildUsers = lib.mkForce true;

  # Administrative users on Darwin systems are part of the admin group.
  nix.settings.trusted-users = ["@admin"];

  nix.distributedBuilds = lib.mkDefault true;

  # FIXME: needs flake-compat
  # nix.nixPath = mkBefore ["darwin-config=${self}"];

  # These UI-enhancement plugins come at an even higher performance cost than
  # completion and do not belong in system configuration at all.
  programs.zsh.enableFzfCompletion = lib.mkForce false;
  programs.zsh.enableFzfGit = lib.mkForce false;
  programs.zsh.enableFzfHistory = lib.mkForce false;
  programs.zsh.enableSyntaxHighlighting = lib.mkForce false;

  environment.systemPackages = with pkgs; [
    m-cli
    mas
    # prefmanager
  ];

  environment.systemPath = lib.mkBefore ["$HOME/.local/bin"];

  # Link native /bin/stty to user bin directory
  # In some rare cases, GNU coreutils stty causes issues on Darwin.
  #
  # Requires that $HOME/.local/bin be added to user's $PATH.
  #
  # <https://github.com/akinomyoga/ble.sh/issues/63#issuecomment-715305422>
  system.activationScripts.postUserActivation.text = ''
    userBin="$HOME/.local/bin"
    target="''${userBin}/stty"

    if [[ ! -d "$userBin" ]]; then
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$userBin"
    fi

    if [[ ! -h "$target" ]]; then
      $DRY_RUN_CMD ln -sf $VERBOSE_ARG /bin/stty "$target"
    fi
  '';

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
