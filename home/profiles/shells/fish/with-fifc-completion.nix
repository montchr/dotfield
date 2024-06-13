{ config, flake, ... }:
{
  # Unreleased: https://github.com/gazorby/fifc/issues/31
  programs.fish.plugins = [ ({ inherit (flake.packages.fish-plugin-fifc) name src; }) ];
  programs.fish.interactiveShellInit = ''
    # Required, apparently...
    set -x fifc_editor ${config.home.sessionVariables."EDITOR"}

    set -x fifc_fd_opts --hidden
  '';
}
