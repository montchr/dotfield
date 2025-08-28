# nix-index :: Nix-oriented package search tool and `command-not-found` replacement
#
# ::: {.note}
# `nix-index` is useful in itself, but fish shell *needs* it, as
# `command-not-found` simply spits out errors.
# :::
#
# <https://github.com/nix-community/nix-index>

{ inputs, ... }:
{
  aspects.core.home = {
    imports = [
      inputs.nix-index-database.homeModules.nix-index
    ];

    programs.command-not-found.enable = false;
    # FIXME: the shell integration script for nix-index is probably sourced twice
    # when home-manager is loaded as a system module
    programs.nix-index.enable = true;
    programs.nix-index.symlinkToCacheHome = true;
    programs.nix-index-database.comma.enable = true;
  };
}
