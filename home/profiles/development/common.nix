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
    pkgs.ast-grep
    pkgs.quicktype # json schema toolkit
    pkgs.universal-ctags

    pkgs.jq-lsp

    # checkers & formatters {{{
    pkgs.biome
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.yamllint
    # }}}

  ]
  ++ lib.optionals config.programs.fish.enable [ pkgs.fish-lsp ];
}
