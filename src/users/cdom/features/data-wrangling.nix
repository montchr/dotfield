{
  dotfield.modules.development.home =
    { pkgs, ... }:
    {
      home.packages = [
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
    };
}
