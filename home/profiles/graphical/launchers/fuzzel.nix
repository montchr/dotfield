{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.programs.fuzzel.package;
in
{
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      terminal = "kitty";
      layer = "overlay";
    };
  };
  wayland.windowManager.sway.config.menu = lib.getExe pkgs.fuzzel;
}
