{ pkgs, ... }:
{
  imports = [
    ./nix-tools.nix
    ./nodejs.nix
  ];

  home.packages = [
    pkgs.asciinema
    pkgs.ast-grep
    pkgs.lynis # security auditing
    pkgs.universal-ctags

    # {{{ checkers & formatters
    pkgs.biome
    pkgs.dotenv-linter
    pkgs.nodePackages.prettier
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.yamllint
    # }}}
  ];
}
