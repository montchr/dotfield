{pkgs, ...}: {
  imports = [
    ./difftools/delta.nix
    ./difftools/difftastic.nix
  ];

  home.packages = [
    pkgs.asciinema
    pkgs.hyperfine
    pkgs.just
    pkgs.watchexec

    # TODO: evaluate benefits/drawbacks -- do we need this?
    pkgs.universal-ctags

    ## === Data and Documents ===

    pkgs.xsv # A fast CSV command line toolkit

    ## === Linters + Formatters ===

    pkgs.shfmt
    pkgs.shellcheck
    pkgs.treefmt #   <- one cli to format the code tree
    pkgs.yamllint
  ];
}
