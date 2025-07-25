{ lib, pkgs, ... }:
{
  # https://jj-vcs.github.io/jj/latest/config/#generating-diffs-by-external-command
  programs.jujutsu.settings.ui.diff-formatter = [
    (lib.getExe pkgs.difftastic)
    "--color=always"
    "$left"
    "$right"
  ];

}
