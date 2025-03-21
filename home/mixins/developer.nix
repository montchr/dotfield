{ pkgs, ... }:
{
  imports = [
    ../profiles/development/common.nix
    ../profiles/development/difftools/delta.nix
    ../profiles/development/data-wrangling.nix
    ../profiles/development/nix-tools.nix
    ../profiles/development/nodejs.nix

    ../profiles/cheatsheets.nix
    ../profiles/emacs/default.nix
    ../profiles/jujutsu.nix
    ../profiles/just.nix
  ];

  home.packages = [
    pkgs.copier
    pkgs.git-filter-repo
    pkgs.vscode
  ];
}
