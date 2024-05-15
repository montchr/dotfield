{ pkgs, ... }:
{
  home.packages = [
    # "Miller is like awk, sed, cut, join, and sort for name-indexed data such
    # as CSV, TSV, and tabular JSON"
    # <https://github.com/johnkerl/miller>
    pkgs.miller
    # TODO: not yet: <https://github.com/NixOS/nixpkgs/pull/296424>
    # pkgs.qsv # "CSVs sliced, diced & analyzed" (maintained fork of xsv)
    pkgs.tidy-viewer # `tv` => Pretty-print CSV files

    pkgs.xlsx2csv # Convert xslx to csv => <https://github.com/dilshod/xlsx2csv>
    pkgs.visidata # better than Excel?? <https://www.visidata.org/>
  ];
}
