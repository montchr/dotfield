{ self, ... }:
{
  dotfield.features.wayland-wm.home =
    { config, pkgs, ... }:
    let
      prefs = self.dotfield.meta.users.${config.home.username}.preferences;
    in
    {
      programs.fuzzel.enable = true;
      programs.fuzzel.settings = {
        main = {
          launch-prefix = "uwsm app -- ";
          use-bold = true;
          terminal = prefs.term;
          layer = "overlay";
        };
      };
      home.packages = [ pkgs.fuzzel ];
    };
}
