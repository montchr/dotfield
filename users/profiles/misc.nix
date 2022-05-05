{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # TODO: fails to build on darwin as of 2022-05-04
    # nodePackages.mermaid-cli # https://github.com/mermaid-js/mermaid-cli
    pandoc

    ## === Local Development ===

    act # Run GitHub Actions locally
    asciinema
    circleci-cli
    hyperfine
    universal-ctags
    yq

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
