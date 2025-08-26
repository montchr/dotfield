flake@{ lib, ... }:
{
  dotfield.aspects.development.home =
    { config, pkgs, ... }:
    let
      checkerPkgs = [
        pkgs.biome
        pkgs.shellcheck
      ];

      demoPkgs = [
        pkgs.asciinema
        pkgs.vhs
      ];

      formatterPkgs = [
        pkgs.biome
        pkgs.shfmt
      ];

      languageServerPkgs = [
        pkgs.jq-lsp
        pkgs.yaml-language-server
      ];
    in
    {
      home.packages = [
        pkgs.ast-grep
        pkgs.csvkit
        pkgs.quicktype # json schema toolkit
        pkgs.universal-ctags
      ]
      ++ checkerPkgs
      ++ demoPkgs
      ++ formatterPkgs
      ++ languageServerPkgs
      ++ lib.optionals config.programs.fish.enable [ pkgs.fish-lsp ];

      # NOTE: This will significantly slow down builds.  However, it enables more
      # manpage integrations across various tools (e.g. `apropos`, `man -k`).
      programs.man.generateCaches = true;
    };
}
