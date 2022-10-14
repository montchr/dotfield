{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) toString;
  inherit (lib) mkOption;
  inherit (lib.types) path str;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  cfg = config.dotfield;
in {
  imports = [
    ./guardian.nix
  ];
  options.dotfield = {
    paths = {
      # FIXME: won't this still end up referencing the previous generation in the store?
      flakeRoot = mkOption {
        type = path;
        apply = toString;
        readOnly = true;
      };
      fsPath = mkOption {
        type = str;
        # NOTE: This module will not be loaded by a home-manager configuration,
        # so we need only be concerned with NixOS and nix-darwin. In the latter
        # case, it is reasonably safe to assume that `$HOME` will be set and
        # that an administrative user has already cloned the flake into place.
        default =
          if isDarwin
          then "$HOME/.config/dotfield"
          else "/etc/dotfield";
      };
    };
  };
  config = {
    dotfield.paths.flakeRoot = ../../.;
    home-manager.sharedModules = [
      {
        home.sessionVariables."DOTFIELD_DIR" = cfg.paths.fsPath;
      }
    ];
  };
}
