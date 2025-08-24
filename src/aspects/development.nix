{ lib, ... }:
{
  dotfield.aspects.development.home =
    { config, pkgs, ... }:
    {
      home.packages = [
        pkgs.asciinema
        pkgs.biome
        pkgs.nixpkgs-review
        pkgs.nixfmt-rfc-style
        pkgs.shellcheck
        pkgs.treefmt
      ]
      ++ lib.optionals config.programs.fish.enable [ pkgs.fish-lsp ];

      # NOTE: This will significantly slow down builds.  However, it enables more
      # manpage integrations across various tools (e.g. `apropos`, `man -k`).
      programs.man.generateCaches = true;
    };
}
