flake@{ ... }:
let
  inherit (flake.config.dotfield.meta.users.cdom) whoami;
in
{
  dotfield.users.cdom.aspects.development.home =
    { config, pkgs, ... }:
    {
      home.packages = [
        pkgs.diffedit3
      ];

      programs.jujutsu.settings = {
        user = {
          inherit (whoami) email name;
        };

        ui = {
          # For interoperability with other tools that don't know jj.
          conflict-marker-style = "git";
          diff-formatter = ":git";

          movement.edit = true;
        };

        template-aliases = {
          # Display relative timestamps in log output
          "format_timestamp(timestamp)" = "timestamp.ago()";
        };
      };
    };
}
