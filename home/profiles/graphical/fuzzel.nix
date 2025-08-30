{
  flake,
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
  programs.fuzzel.settings = {
    main = {
      launch-prefix = "uwsm app -- ";
      use-bold = true;
      terminal = prefs.term or "ghostty";
      layer = "overlay";
    };
  };
  home.packages = [ pkgs.fuzzel ];
}
