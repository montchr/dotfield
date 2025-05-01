{
  flake,
  config,
  pkgs,
  lib,
  ...
}:
let
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
in
{
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      launch-prefix = "uwsm app -- ";
      use-bold = true;
      terminal = prefs.term or "foot";
      layer = "overlay";
    };
  };
  home.packages = [ pkgs.fuzzel ];
}
