{ pkgs, ... }:
{
  home.packages = [
    pkgs.just
    pkgs.just-lsp
  ];
  home.shellAliases."j" = "just";
}
