{
  config,
  lib,
  pkgs,
  ...
}: let
  shellAbbrs = import ../abbrs.nix;
  shellAliases = import ../aliases.nix;

  mkPlugin = name: {
    inherit name;
    inherit (pkgs.sources."fish-${name}") src;
  };
  mkPlugins = plugins: (map mkPlugin plugins);
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    fishPlugins.done
    fishPlugins.forgit
    fishPlugins.fzf-fish
  ];

  programs.fish = {
    inherit
      # FIXME: watch out, some of these abbrs may have undesirable results when
      # expanded inline. needs review.
      shellAbbrs
      shellAliases
    ;

    enable = true;
    plugins = mkPlugins [
      "autopair"
      "fifc"
      "replay"
    ];
    interactiveShellInit = ''
      # "Required" by `fifc`
      # set -Ux fifc_editor $EDITOR
    '';
  };
}
