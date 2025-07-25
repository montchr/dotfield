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
        # For interoperability with other tools that don't know jj.
        conflict-marker-style = "git";
        diff-formatter = ":git";

        movement.edit = true;
      };
    };
  };
}
