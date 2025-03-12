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
      use-bold = true;
      terminal = prefs.term or "foot";
      layer = "overlay";
    };
  };
  wayland.windowManager.sway.config.menu = lib.getExe pkgs.fuzzel;
  home.packages = [ pkgs.fuzzel ];
}
