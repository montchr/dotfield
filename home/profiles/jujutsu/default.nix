{
  lib,
  pkgs,
  config,
  ...
}:
let
  inherit (config.dotfield) whoami;

  # TODO: for visibility -- refactor "later"
  guiTools = [
    pkgs.gg-jj
    pkgs.diffedit3
  ];
in
{
  imports = [ ./__starship-prompt-support.nix ];

  home.packages = [
    pkgs.jjui
    pkgs.jj-fzf
    pkgs.lazyjj
  ]
  ++ guiTools;

  programs.jujutsu = {
    enable = true;
    settings = {
      aliases = {
        # SYNOPSIS: jj harvest <revset>
        #
        # NOTE: `--from` revset argument is deliberately omitted a la
        # partial application so the argument may be provided in the
        # shell.  Surprisingly, this Just Works™!
        "harvest" = [
          "squash"
          "--interactive"
          "--to"
          "@"
          "--from"
        ];
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
