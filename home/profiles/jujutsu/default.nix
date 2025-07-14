{
  lib,
  pkgs,
  config,
  ...
}:
let
  inherit (config.dotfield) whoami;
in
{
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        inherit (whoami) email name;
      };
      ui = {
        default-command = [
          "log"
          "--reversed"
        ];
        # https://jj-vcs.github.io/jj/latest/config/#generating-diffs-by-external-command
        diff-formatter = [
          (lib.getExe pkgs.difftastic)
          "--color=always"
          "$left"
          "$right"
        ];
        movement.edit = true;
      };
    };
  };
}
