{ lib, ... }:
{
  aspects.development.home =
    { config, pkgs, ... }:
    {
      home.packages =
        let
          dataWrangling = [
            pkgs.csvkit # https://csvkit.readthedocs.io/en/latest/
            pkgs.miller
            pkgs.qsv # "CSVs sliced, diced & analyzed" (maintained fork of xsv)
            pkgs.tidy-viewer # `tv` => Pretty-print CSV files
            pkgs.xan # <- "the CSV magician" => <https://github.com/medialab/xan>
            pkgs.xlsx2csv # Convert xslx to csv => <https://github.com/dilshod/xlsx2csv>
            pkgs.xml2 # Tools for command line processing of XML, HTML, and CSV
            pkgs.xq-xml # XML formatter and scraper

            # XXX(2024-08-14): build failure
            # pkgs.visidata
          ];

          nixTools = [
            pkgs.nix-init
            pkgs.nix-inspect # <- configuration inspector
            pkgs.nixfmt-rfc-style
            pkgs.nixpkgs-review
          ];
        in
        dataWrangling
        ++ nixTools
        ++ [
          pkgs.asciinema
          pkgs.ast-grep
          pkgs.copier
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

      # NOTE: This will significantly slow down builds.  However, it enables more
      # manpage integrations across various tools (e.g. `apropos`, `man -k`).
      programs.man.generateCaches = true;

    };
}
