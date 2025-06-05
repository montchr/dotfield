{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = lib // builtins;

  osCfg = config.dotfield;

  featuresOpt = l.mkOption {
    type = l.types.attrsOf l.types.unspecified;
    default = { };
  };

  # Whether Impermanence aka "ephemeral root storage" aka "darling erasure"
  # is enabled for this configuration.
  #
  # TODO: until impermanence support is added, this should be set to false.
  # for the time being, we use the setting to prepare for impermanence prior
  # to implementation.
  hasImpermanence = false;
in
{
  imports = [
    ./_guardian.nix
    ./_hosts.nix
    ./_users.nix
  ];
  options.dotfield = {
    enable = l.mkOption {
      default = true;
      type = l.types.bool;
    };
    features = featuresOpt;
    paths = {
      fsPath = l.mkOption {
        type = l.types.str;
        default = if isDarwin then "$HOME/.config/dotfield" else "/etc/dotfield";
      };
      persistence = l.mkOption {
        type = l.types.str;
        default = "/persist";
        description = ''
          Absolute path to non-ephemeral file storage.
        '';
      };
      # FIXME: replace with explicit usage of `dotfield.paths.persistence`
      storageBase = l.mkOption {
        type = l.types.str;
        default = if hasImpermanence then "/persist" else "";
        description = ''
          Absolute path to the root directory for non-ephemeral file storage,
          taking impermanence settings into account.
        '';
      };
    };
  };
  config = {
    dotfield = {
      # FIXME: make submodule
      features = {
        inherit hasImpermanence;

        # Whether a NixOS system has enabled the proprietary NVIDIA drivers.
        # FIXME: The default `false` value indicates that we cannot know with
        # certainty whether NVIDIA drives are in use. This may be the case, for
        # example, on generic Linux with a standalone home-manager.
        # FIXME: 2024-07-29 causes eval error on tuvok
        # hasNvidia = config.hardware.nvidia.modesetting.enable or false;
        hasNvidia = lib.mkDefault false;

        # Whether the system has any features indicating a Wayland session.
        hasWayland = config.services.displayManager.gdm.wayland or config.programs.sway.enable or false;
      };
    };

    home-manager.sharedModules = [
      { home.sessionVariables."DOTFIELD_DIR" = osCfg.paths.fsPath; }
      (
        hmArgs:
        let
          hmConfig = hmArgs.config;
          hasSway = hmConfig.wayland.windowManager.sway.enable;
        in
        {
          options.dotfield.features = featuresOpt;
          # FIXME: make submodule
          config.dotfield.features = {
            inherit hasSway;
            hasWayland = osCfg.hasWayland or hasSway;
          };
        }
      )
    ];
  };
}
