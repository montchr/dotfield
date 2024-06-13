{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (lib) mkOption types;

  featuresSubmodule = types.submodule (
    { options, ... }:
    {
      options = {
        hasNvidia = mkOption {
          type = types.bool;
          readOnly = true;
          description = ''
            Whether a NixOS system has enabled the proprietary NVIDIA drivers.
          '';
        };
      };
    }
  );
in
{
  imports = [
    ./_guardian.nix
    ./_hosts.nix
    ./_users.nix
  ];
  options.dotfield = {
    enable = mkOption {
      default = true;
      type = types.bool;
    };
    features = mkOption {
      type = featuresSubmodule;
      default = { };
    };
    paths = {
      fsPath = mkOption {
        type = types.str;
        default = if isDarwin then "$HOME/.config/dotfield" else "/etc/dotfield";
      };
    };
  };
  config = {
    dotfield = {
      features = {
        # FIXME: The default `false` value indicates that we cannot know with
        # certainty whether NVIDIA drives are in use. This may be the case, for
        # example, on generic Linux with a standalone home-manager.
        hasNvidia = config.hardware.nvidia.modesetting.enable or false;
      };
    };
  };
}
