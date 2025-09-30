{ lib, ... }:
{
  aspects.development.home =
    { config, pkgs, ... }:
    {
      programs.mergiraf.enable = true;

      home.packages =
        let
          dataWrangling = [
            pkgs.miller
            pkgs.tidy-viewer # `tv` => Pretty-print CSV files
            pkgs.xml2 # Tools for command line processing of XML, HTML, and CSV
            pkgs.xq-xml # XML formatter and scraper
          ];

          nixTools = [
            pkgs.nix-init
            pkgs.nix-inspect
            pkgs.nixfmt-rfc-style
            pkgs.nixpkgs-review
          ];

          languageServers = [ pkgs.jq-lsp ];
        in
        dataWrangling
        ++ nixTools
        ++ languageServers
        ++ [
          pkgs.asciinema
          pkgs.ast-grep
          pkgs.copier
          pkgs.quicktype # json schema toolkit
          pkgs.universal-ctags
        ];

      # NOTE: This will significantly slow down builds.  However, it enables more
      # manpage integrations across various tools (e.g. `apropos`, `man -k`).
      programs.man.generateCaches = true;
    };
}
