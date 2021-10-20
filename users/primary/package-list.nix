{ pkgs }:
with pkgs; [
  act # Run GitHub Actions locally
  asciinema
  # bandwhich # display current network utilization by process
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
  universal-ctags
  vim-vint
  yamllint
  yq
  zoxide
]
