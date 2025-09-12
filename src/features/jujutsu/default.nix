flake@{ lib, self, ... }:
let
  inherit (self.lib) isEmpty;
in
{
  aspects.development.home =
    {
      pkgs,
      config,
      ...
    }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;

      # TODO: for visibility -- refactor "later"
      guiTools = [
        pkgs.gg-jj
        pkgs.diffedit3
      ];
    in
    {
      imports = [
        self.modules.homeManager."programs/jujutsu/signing"
      ];

      home.packages = [
        pkgs.jjui
      ]
      ++ guiTools;

      # This should be, for now, the developer's responsibility.  It is not
      # on individual projects to add an ignore for somebody's exotic
      # workflow until that exotic workflow becomes widely adopted.  Or do
      # you think that adding this to a project's official gitignore is good
      # publicity for Jujutsu?
      programs.git.ignores = [ ".jj*" ];

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

          user = lib.mkIf (!isEmpty (whoami.name or false) && !isEmpty (whoami.email or false)) {
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
    };
}
