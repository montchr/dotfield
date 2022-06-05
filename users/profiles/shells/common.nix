{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.dotfield.whoami) githubUserName;

  envInit = import ./env-init.sh.nix;

  shellAliases =
    (import ./abbrs.nix)
    // (import ./aliases.nix);

  fdBin = "${pkgs.fd}/bin/fd";
in {
  imports = [
    ./fzf.nix
    ./starship.nix
  ];

  programs.bash = {
    inherit shellAliases;

    enable = true;
    bashrcExtra = envInit;
    # profileExtra = "";

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };

  # Magical shell history
  programs.atuin.enable = true;
  programs.atuin.settings = {
    auto_sync = true;
    sync_frequency = "30m";
    search_mode = "fuzzy"; # 'prefix' | 'fulltext' | 'fuzzy'
    filter_mode = "global"; # 'global' | 'host' | 'session' | 'directory'
  };
}
