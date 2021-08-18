{ config, inputs, lib, pkgs, ... }: {
  imports = [ ./macos.nix ];

  nix = { trustedUsers = [ "@admin" ]; };

  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;

  my.modules = {
    macos.enable = true;
    gpg.enable = true;
    editors.emacs.enable = true;
  };

  my.hm.configFile = {
    "brew" = {
      source = "${config.dotfield.flkConfigDir}/brew";
      recursive = true;
    };
  };

  homebrew = {
    enable = true;
    autoUpdate = true;
    global.noLock = true;
    # TODO
    # cleanup = "zap";
    # global.brewfile = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.stateVersion
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
