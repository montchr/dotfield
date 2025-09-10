{
  users.cdom.aspects.core.home =
    { lib, pkgs, ... }:
    {
      programs.yazi = {
        enable = true;
        shellWrapperName = "y";
        plugins =
          [
            pkgs.yaziPlugins.chmod
            pkgs.yaziPlugins.git
            pkgs.yaziPlugins.glow
            pkgs.yaziPlugins.mediainfo
            pkgs.yaziPlugins.ouch
            pkgs.yaziPlugins.projects
            pkgs.yaziPlugins.rsync
          ]
          |> lib.map (pkg: {
            name = lib.removeSuffix ".yazi" pkg.pname;
            value = pkg;
          })
          |> lib.listToAttrs;
      };
    };
}
