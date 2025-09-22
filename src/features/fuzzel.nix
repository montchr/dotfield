flake@{ ... }:
{
  aspects.graphical.home =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      prefs = flake.config.meta.users.${config.home.username}.preferences;
    in
    {
      programs.fuzzel.enable = true;
      programs.fuzzel.settings.main = {
        terminal = prefs.term;
      };
      home.packages = [ pkgs.fuzzel ];
    };
}
