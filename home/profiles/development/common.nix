{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./nix-tools.nix
    ./nodejs.nix
  ];

  # NOTE: This will significantly slow down builds.  However, it enables more
  # manpage integrations across various tools (e.g. `apropos`, `man -k`).
  programs.man.generateCaches = true;

  home.packages = [
    pkgs.asciinema
    # FIXME: build failure
    # pkgs.ast-grep
    pkgs.lynis # security auditing
    pkgs.quicktype # json schema toolkit
    pkgs.universal-ctags

    pkgs.jq-lsp

    # checkers & formatters {{{
    pkgs.biome
    pkgs.dotenv-linter
    pkgs.nodePackages.prettier
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.yamllint
    # }}}

  ] ++ lib.optionals config.programs.fish.enable [ pkgs.fish-lsp ];
}
