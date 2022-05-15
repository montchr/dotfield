{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.tmux = {
    enable = true;
    clock24 = true;
    keyMode = "vi";
  };
}
