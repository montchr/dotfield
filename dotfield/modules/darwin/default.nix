{ config, inputs, lib, pkgs, ... }: {
  nix = { trustedUsers = [ "@admin" ]; };

  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;

  my = {
    modules = {
      macos.enable = true;
      gpg.enable = true;
      editors.emacs.enable = true;
    };
  };

  homebrew = {
    enable = true;
    autoUpdate = true;
    # TODO
    # cleanup = "zap";
    # global.brewfile = true;
    global.noLock = true;
  };

  imports = [ ./macos.nix ];

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
