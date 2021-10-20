{ config, inputs, lib, pkgs, ... }: {
  imports = [ ./macos.nix ];

  nix.trustedUsers = [ "@admin" ];

  my.modules = {
    macos.enable = true;
    gpg.enable = true;
    editors.emacs.enable = true;
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
