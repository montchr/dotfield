{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [

    ## === Local Development ===

    act # Run GitHub Actions locally
    asciinema
    circleci-cli
    hyperfine
    universal-ctags

    ## === Data and Documents ===

    pandoc
    tidy-viewer # Pretty-print CSV files
    visidata # A terminal spreadsheet multitool for discovering and arranging data
    xsv # A fast CSV command line toolkit
    yq

    # TODO: fails to build on darwin as of 2022-05-04
    # nodePackages.mermaid-cli # https://github.com/mermaid-js/mermaid-cli

    ## === Linters + Formatters ===

    shfmt
    shellcheck
    yamllint
  ];

  # TODO: check these out
  # services.etebase = {};
  # services.flameshot = {};
  # services.git-sync = {};
  # services.password-store-sync = {};
  # programs.ncspot = {}; # spotify thingy
}
