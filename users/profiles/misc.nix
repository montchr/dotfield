{
  config,
  lib,
  pkgs,
  ...
}: {
  lib.dotfield.userPath = "${config.xdg.configHome}/dotfield";
  lib.dotfield.whoami = rec {
    firstName = "Chris";
    lastName = "Montgomery";
    fullName = "${firstName} ${lastName}";
    email = "chris@cdom.io";
    githubUserName = "montchr";
    pgpPublicKey = "0x135EEDD0F71934F3";
  };

  home.sessionVariables = {
    DOTFIELD_DIR = config.lib.dotfield.userPath;
  };

  home.packages = with pkgs; [
    act # Run GitHub Actions locally
    asciinema
    cacert
    circleci-cli
    du-dust
    getopt
    grex # Generate regexps from user-provided test cases

    # Modern, user-friendly command-line HTTP client for the API era.
    # https://httpie.io/docs/cli
    httpie

    hyperfine
    lnav # System Log file navigator
    mcfly
    pandoc
    podman
    shellcheck
    shfmt
    starship
    universal-ctags
    vim-vint
    # https://ergodox-ez.com/pages/wally-planck
    wally-cli
    yamllint
    yq
  ];
}
