{ flake, ... }:
let
  inherit (flake.inputs) nix-index-database;
in
{
  imports = [ nix-index-database.nixosModules.nix-index ];
  # Nix-oriented package search tool and `command-not-found` replacement.
  #
  # `nix-index` is useful in itself, but fish shell *needs* it, as
  # `command-not-found` simply spits out errors.
  #
  # <https://github.com/nix-community/nix-index>
  #
  programs.command-not-found.enable = false;
  programs.nix-index.enable = true;
  # NOTE: This will install the `comma` command-line tool.
  programs.nix-index-database.comma.enable = true;
}
