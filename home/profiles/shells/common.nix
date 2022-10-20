{
  config,
  lib,
  pkgs,
  ...
}: let
  shellAliases =
    (import ./abbrs.nix)
    // (import ./aliases.nix);
in {
  imports = [
    ./fzf.nix
    ./starship.nix
  ];

  home.packages = [
    (pkgs.writeShellScriptBin "md" ''
      [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1"
    '')
  ];

  programs.bash = {
    inherit shellAliases;

    enable = true;
    # profileExtra = "";

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };

  # Magical shell history
  programs.atuin.enable = true;
  programs.atuin.settings = {
    auto_sync = true;
    sync_frequency = "10m";
    sync_address = "https://api.atuin.sh";
    search_mode = "fuzzy"; # 'prefix' | 'fulltext' | 'fuzzy'
    filter_mode = "global"; # 'global' | 'host' | 'session' | 'directory'
  };
}
