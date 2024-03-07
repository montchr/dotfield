# Prevent activation failure during boot when using the NixOS module and
# network is not yet available.
#
# FIXME: Remove once merged:
# <https://github.com/nix-community/home-manager/pull/5005>
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.programs.tealdeer;

  configDir = if pkgs.stdenv.isDarwin then "Library/Application Support" else config.xdg.configHome;

  tomlFormat = pkgs.formats.toml { };

  settingsFormat =
    let
      updatesSection = types.submodule {
        options = {
          auto_update = mkOption {
            type = types.bool;
            default = false;
            defaultText = literalExpression "false";
            example = literalExpression "true";
            description = ''
              Specifies whether the auto-update feature should be enabled.
            '';
          };
          auto_update_interval_hours = mkOption {
            type = types.ints.positive;
            default = 720;
            defaultText = literalExpression "720";
            example = literalExpression "24";
            description = ''
              Duration, since the last cache update, after which the cache will be refreshed.
              This parameter is ignored if {var}`auto_update` is set to `false`.
            '';
          };
        };
      };
    in
    types.submodule {
      freeformType = tomlFormat.type;
      options = {
        updates = mkOption {
          type = updatesSection;
          default = { };
          description = ''
            Tealdeer can refresh the cache automatically when it is outdated.
            This behavior can be configured in the updates section.
          '';
        };
      };
    };
in
{
  meta.maintainers = [ hm.maintainers.pedorich-n ];

  # Use our local fork of these modules while still pending upstream changes.
  disabledModules = [ "programs/tealdeer.nix" ];

  options.programs.tealdeer = {
    enable = mkEnableOption "Tealdeer";

    settings = mkOption {
      type = settingsFormat;
      default = { };
      defaultText = literalExpression "{ }";
      example = literalExpression ''
        {
          display = {
            compact = false;
            use_pager = true;
          };
          updates = {
            auto_update = false;
          };
        };
      '';
      description = ''
        Configuration written to
        {file}`$XDG_CONFIG_HOME/tealdeer/config.toml` on Linux or
        {file}`$HOME/Library/Application Support/tealdeer/config.toml` on Darwin.
        See <https://dbrgn.github.io/tealdeer/config.html> for more information.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.tealdeer ];

    home.file."${configDir}/tealdeer/config.toml" = mkIf (cfg.settings != { }) {
      source = tomlFormat.generate "tealdeer-config" cfg.settings;
    };
  };
}
