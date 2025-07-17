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
  imports = [ ./__starship-prompt-support.nix ];

  home.packages = [
    pkgs.jjui
    pkgs.jj-fzf
  ];

  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        "l" = [
          "log"
          "--no-pager"
          "--limit=6"
        ];
        "s" = [
          "st"
          "--no-pager"
        ];
      };
      template-aliases = {
        # Display relative timestamps in log output
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      user = {
        inherit (whoami) email name;
      };
      ui = {
        # default-command = [
        #   "log"
        # ];
        # https://jj-vcs.github.io/jj/latest/config/#generating-diffs-by-external-command
        diff-formatter = [
          (lib.getExe pkgs.difftastic)
          "--color=always"
          "$left"
          "$right"
        ];
        movement.edit = true;
        # paginate = "never";
      };
    };
  };
}
