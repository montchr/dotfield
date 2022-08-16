{
  config,
  lib,
  pkgs,
  profiles,
  ...
}: let
  inherit (lib.types) nullOr str submodule;
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;

  cfg = config.dotfield;

  hostModule = submodule (import ./host.nix);

  guardianModule = submodule {
    options = {
      enable = lib.mkEnableOption "Whether to designate a guardian user for this system.";
      username = lib.mkOption {
        type = nullOr str;
        default = null;
        # FIXME: validate that this is an existing user.
        description = ''
          Name of the guardian user. Must be an existing non-system user.
        '';
      };
      user = lib.mkOption {
        readOnly = true;
      };
      autoLogin = lib.mkEnableOption "Whether to log the guardian user in automatically.";
    };
  };
in {
  options.dotfield = {
    guardian = lib.mkOption {
      type = guardianModule;
      description = ''
        A primary user for this system.
      '';
    };
    host = lib.mkOption {
      type = hostModule;
    };
  };
  config = {
    # dotfield = {
    #   guardian =
    #     lib.mkIf cfg.enable {
    #     };
    # };

    dotfield.guardian.user = lib.mkAliasDefinitions config.users.users.${cfg.guardian.username};

    users.groups."wheel".members = lib.mkIf cfg.guardian.enable [cfg.guardian.username];
  };
}
# { config, lib, pkgs, ... }:
# let
#   inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;
#   t = with lib.types; either str path;
# in
# {
#   options = {
#     dotfield = rec {
#       configDir = mkOpt t "${config.dotfield.dir}/config";
#       dir = mkOpt t (toString ../.);
#       # FIXME: This points to an arbitrary location which may vary per system.
#       # Instead, it should be determined programmatically based on the flake's
#       # actual location. Note, however, that its currently only useful for
#       # out-of-store symlinks, which are generally discouraged as they do not
#       # adhere to the Nix principle of immutable configuration.
#       path = mkOpt t "${config.my.user.home}/.config/dotfield";
#       binDir = mkOpt t "${config.dotfield.dir}/bin";
#       libDir = mkOpt t "${config.dotfield.dir}/lib";
#       modulesDir = mkOpt t "${config.dotfield.dir}/modules";
#       vendorDir = mkOpt t "${config.dotfield.dir}/vendor";
#     };
#   };
# }
