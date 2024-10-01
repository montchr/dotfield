{ pkgs, ... }:
{
  imports = [
    ../profiles/development/difftools/delta.nix
    ../profiles/emacs/default.nix
    ../profiles/just.nix
  ];

  home.packages = [
    pkgs.git-filter-repo
    pkgs.vscode
  ];
}
