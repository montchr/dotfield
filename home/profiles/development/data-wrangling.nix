{ pkgs, ... }:
{
  home.packages = [
    pkgs.csvkit # https://csvkit.readthedocs.io/en/latest/
    pkgs.miller
    pkgs.qsv # "CSVs sliced, diced & analyzed" (maintained fork of xsv)
    pkgs.tidy-viewer # `tv` => Pretty-print CSV files
    pkgs.xlsx2csv # Convert xslx to csv => <https://github.com/dilshod/xlsx2csv>
    pkgs.xidel # XML document scraper
    pkgs.xml2 # Tools for command line processing of XML, HTML, and CSV

    # XXX(2024-08-14): build failure
    # pkgs.visidata
  ];
}
