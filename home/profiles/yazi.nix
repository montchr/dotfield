{ pkgs, ... }:
{
  programs.yazi = {
    enable = true;
    shellWrapperName = "y";
    plugins = builtins.listToAttrs (
      builtins.map
        (pkg: {
          name = pkg.pname;
          value = pkg;
        })
        [
          pkgs.yaziPlugins.chmod
          pkgs.yaziPlugins.git
          pkgs.yaziPlugins.glow
          pkgs.yaziPlugins.mediainfo
          pkgs.yaziPlugins.ouch
          pkgs.yaziPlugins.projects
          pkgs.yaziPlugins.rsync
        ]
    );
  };
}
