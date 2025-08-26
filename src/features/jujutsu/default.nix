{ lib, ... }:
{
  dotfield.aspects.development.home =
    { config, pkgs, ... }:
    {
      home.packages = [ pkgs.jjui ];

      programs.jujutsu.enable = true;

      programs.jujutsu.settings.ui = {
        # For interoperability with other tools that don't know jj.
        conflict-marker-style = lib.mkDefault "git";
        diff-formatter = lib.mkDefault ":git";
      };

      # This should be, for now, the operator's responsibility.  It is not
      # on individual projects to add an ignore for somebody's exotic
      # workflow until that exotic workflow becomes widely adopted.  Or do
      # you think that adding this to a project's official gitignore is good
      # publicity for Jujutsu?  Maybe it is.
      programs.git.ignores = [ ".jj*" ];

    };
}
