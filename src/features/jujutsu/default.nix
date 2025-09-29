flake@{
  inputs,
  lib,
  self,
  ...
}:
let
  inherit (inputs.apparat.lib) isEmpty;
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
    in
    {
      imports = [
        self.modules.homeManager.jujutsu-signing
      ];

      home.packages = [
        pkgs.jjui
      ];

      # This should be, for now, the developer's responsibility.  It is not
      # on individual projects to add an ignore for somebody's exotic
      # workflow until that exotic workflow becomes widely adopted.  Or do
      # you think that adding this to a project's official gitignore is good
      # publicity for Jujutsu?
      programs.git.ignores = [ ".jj*" ];

      programs.jujutsu = {
        enable = true;
        settings = {
          user = {
            name = whoami.name or "";
            email = whoami.email or "";
          };
          ui = {
            # For interoperability with other tools that don't know jujutsu.
            conflict-marker-style = "git";
            diff-formatter = ":git";
          };
        };
      };
    };
}
