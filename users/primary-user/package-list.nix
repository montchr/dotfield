{ pkgs }:
with pkgs; [
  act # Run GitHub Actions locally
  asciinema
  bandwhich # display current network utilization by process
  cacert
  circleci-cli
  du-dust
  getopt
  grex # Generate regexps from user-provided test cases
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
]
