{ pkgs, ... }:
{
  # FIXME: most of these should NOT be considered part of "core"
  # most don't belong on a server, etc.
  home.packages = with pkgs; [
    ## === Local Development ===

    asciinema
    hyperfine
    universal-ctags

    ## === Data and Documents ===

    tidy-viewer # Pretty-print CSV files
    xlsx2csv # Convert xslx to csv => <https://github.com/dilshod/xlsx2csv>
    xsv # A fast CSV command line toolkit
    # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
    python3Packages.yq

    ## === Linters + Formatters ===

    shfmt
    shellcheck
    yamllint
  ];

  # TODO: check these out
  # services.etebase = {};
  # services.etebase.sync = {};
  # services.flameshot = {};
  # services.hound = ...
}
