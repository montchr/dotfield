{ pkgs, ... }:
{
  imports = [
    ./nix-tools.nix
    ./nodejs.nix
  ];

  home.packages = [
    pkgs.asciinema
    pkgs.dotenv-linter
    pkgs.nodePackages.prettier
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.universal-ctags
    pkgs.yamllint
  ];
}
