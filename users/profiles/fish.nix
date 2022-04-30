# FIXME: known to be broken, but not tough to fix
{
  config,
  lib,
  pkgs,
  ...
}: let
  shellCfg = config.shell;
  mkPlugins = plugins: (map
    (name: {
      inherit name;
      inherit (pkgs.sources."fish-${name}") src;
    })
    plugins);
in {
  # imports = [
  #   ../shell
  # ];

  # my.hm.programs = {
  #   fish = {
  #     enable = true;
  #     interactiveShellInit = ''
  #       set -g fish_greeting
  #     '';
  #     shellInit = ''
  #       for p in /run/current-system/sw/bin
  #         if not contains $p $fish_user_paths
  #           set -U fish_user_paths $p $fish_user_paths
  #         end
  #       end

  #       set -U fish_user_paths /run/wrappers/bin $fish_user_paths

  #       function fish_title
  #         echo "$PWD | $_" | sed "s|$HOME|~|g"
  #       end
  #     '';
  #     shellAbbrs = shellCfg.abbrs;
  #     shellAliases = shellCfg.aliases;
  #     plugins = mkPlugins [
  #       "abbr-tips"
  #       "autopair"
  #       "done"
  #       "fzf"
  #       "nix-env"
  #       "nvm"
  #       "replay"
  #       "z"
  #     ];
  #   };
  #   starship.enableFishIntegration = true;
  # };
}
