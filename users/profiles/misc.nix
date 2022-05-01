{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
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
