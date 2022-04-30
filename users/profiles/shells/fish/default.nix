{
  config,
  lib,
  pkgs,
  ...
}: let
  shellAbbrs = import ../abbrs.nix;
  shellAliases = import ../aliases.nix;
  # mkPlugin = name: {
  #   inherit name;
  #   inherit (pkgs.sources."fish-${name}") src;
  # };
  # mkPlugins = plugins: (map mkPlugin plugins);
in {
  imports = [../common.nix];

  programs.fish = {
    inherit shellAbbrs shellAliases;
    # inherit shellAliases;

    enable = true;

    # plugins = mkPlugins [
    #   "abbr-tips"
    #   "autopair"
    #   "done"
    #   "fzf"
    #   "nix-env"
    #   "nvm"
    #   "replay"
    #   "z"
    # ];
  };
}
