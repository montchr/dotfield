{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = lib // builtins;

  osCfg = config.dotfield;

  featuresOpt = l.mkOption {
    type = l.types.attrsOf l.types.unspecified;
    default = {};
  };

  # Whether Impermanence aka "ephemeral root storage" aka "darling erasure"
  # is enabled for this configuration.
  #
  # TODO: until impermanence support is added, this should be set to false.
  # for the time being, we use the setting to prepare for impermanence prior
  # to implementation.
  hasImpermanence = false;
in {
  imports = [
    ./guardian.nix
  ];
  options.dotfield = {
    features = featuresOpt;
    paths = {
      fsPath = l.mkOption {
        type = l.types.str;
        default =
          if isDarwin
          then "$HOME/.config/dotfield"
          else "/etc/dotfield";
      };
      storageBase = l.mkOption {
        type = l.types.str;
        default =
          if hasImpermanence
          then "/persist"
          else "";
        description = ''
          Absolute path to the root directory for non-ephemeral file storage,
          taking impermanence settings into account.
        '';
      };
    };
  };
  config = {
    dotfield = {
      features = {
        inherit hasImpermanence;

        # Whether a NixOS system has enabled the proprietary NVIDIA drivers.
        # FIXME: The default `false` value indicates that we cannot know with
        # certainty whether NVIDIA drives are in use. This may be the case, for
        # example, on generic Linux with a standalone home-manager.
        hasNvidia = config.hardware.nvidia.modesetting.enable or false;

        # Whether the system has any features indicating a Wayland session.
        hasWayland =
          config.services.xserver.displayManager.gdm.wayland
          or config.programs.sway.enable
          or false;
      };
    };

    home-manager.sharedModules = [
      {home.sessionVariables."DOTFIELD_DIR" = osCfg.paths.fsPath;}
      (hmArgs: let
        hmConfig = hmArgs.config;
        hasSway = hmConfig.wayland.windowManager.sway.enable;
      in {
        options.dotfield.features = featuresOpt;
        config.dotfield.features = {
          inherit hasSway;
          hasWayland = osCfg.hasWayland or hasSway;
        };
      })
    ];
  };
}
