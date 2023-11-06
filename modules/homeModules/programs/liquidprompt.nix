# TODO: throw when starship (et al) and its shell integration is enabled -- can
# only be one, but starship could also be e.g. enabled for zsh but disabled for
# bash, and that's fine
{
  config,
  flake,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  # TODO: consider as global module args?
  l = flake.inputs.apparat.lib;
  cfg = config.programs.liquidprompt;
  shellInit = ''
    if [[ $TERM != "dumb" ]]; then
      source ${cfg.package}/bin/liquidprompt
    fi
  '';
in {
  options.programs.liquidprompt = {
    enable = lib.mkEnableOption "liquidprompt";

    package = lib.mkPackageOption pkgs "liquidprompt" {};

    enableBashIntegration = l.mkDisableOption "Bash integration";
    enableZshIntegration = l.mkDisableOption "Zsh integration";

    # FIXME: does nothing, needs processing for config file output (see config below)
    settings = lib.mkOption {
      type = with types; attrsOf (oneOf [bool int path str]);
      default = {};
    };
  };

  config = lib.mkIf cfg.enable {
    # TODO: prefix vars with `LP_` automatically
    # TODO: convert t/f to 1/0
    # TODO: prob need a custom generator via lib.generators.toKeyValue
    # xdg.configFile."liquidpromptrc".text = lib.toShellVars cfg.settings;

    programs.bash.initExtra = shellInit;
    programs.zsh.initExtra = shellInit;
  };
}
