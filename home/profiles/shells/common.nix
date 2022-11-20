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

  programs.bottom.enable = true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.less.enable = true;
  programs.zoxide.enable = true;
}
