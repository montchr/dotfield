{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.dotfield) treesWithEnabledLeaf;

  /*
  hasEnabledModule :: [String] -> AttrSet -> Bool

  Whether any home-manager user on the system has enabled the module at
  the given path.

  Example:

  ```
  {
    home-manager.users = {
      foo = { programs.emacs.enable = true; };
      bar = { programs.neovim.enable = true; };
    };
    programs.emacs.enable = hasEnabledModule ["programs" "emacs"];
  }
  ```
  */
  hasEnabledModule = path: let
    trees = treesWithEnabledLeaf (path ++ ["enable"]) config.home-manager.users;
  in ((builtins.length trees) >= 1);

  /*
  hasWm :: String -> Bool

  Whether the given window manager is enabled by any user.
  */
  hasWm = name: hasEnabledModule ["wayland" "windowManager" name];

  # Whether Impermanence aka "ephemeral root storage" aka "darling erasure"
  # is enabled for this configuration.
  #
  # TODO: until impermanence support is added, this should be set to false.
  # for the time being, we use the setting to prepare for impermanence prior
  # to implementation.
  hasImpermanence = false;

  # Absolute path to the default persistent storage root directory.
  persistentStorageBase = "/persist";
in {
  lib.dotfield = {
    sys = {
      inherit hasImpermanence persistentStorageBase;

      hasHidpi = config.hardware.video.hidpi.enable or false;

      # Whether a NixOS system has enabled the proprietary NVIDIA drivers.
      #
      # FIXME: The default `false` value indicates that we cannot know with
      # certainty whether NVIDIA drives are in use. This may be the case, for
      # example, on generic Linux with a standalone home-manager.
      hasNvidia = config.hardware.nvidia.modesetting.enable or false;

      # Absolute path to the root directory for non-ephemeral file storage,
      # taking impermanence settings into account.
      storageBase =
        if hasImpermanence
        then persistentStorageBase
        else "";

      # Whether a tiling window manager is enabled system-wide.
      hasTwm =
        config.services.yabai.enable
        or config.programs.sway.enable
        or false;

      # Whether the system has any features indicating a Wayland session.
      hasWayland =
        config.services.xserver.displayManager.gdm.wayland
        or config.programs.sway.enable
        or false;
    };

    home = {inherit hasEnabledModule hasWm;};
  };
}
