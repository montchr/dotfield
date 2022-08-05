{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) map;
  inherit (config.lib.fish) mkPlugin;

  shellAbbrs = import ../abbrs.nix;
  shellAliases = import ../aliases.nix;
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    fishPlugins.done
    fishPlugins.forgit
  ];

  programs.fish = {
    inherit
      # FIXME: watch out, some of these abbrs may have undesirable results when
      # expanded inline. needs review.
      shellAbbrs
      shellAliases
      ;

    enable = true;
    autopair.enable = true;
    fifc.enable = true;

    plugins = map mkPlugin [
      "replay"
    ];

    interactiveShellInit = ''
      # "Required" by `fifc`
      # set -Ux fifc_editor $EDITOR
    '';
  };
}
