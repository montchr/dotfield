{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (config.theme) font;
  cfg = config.programs.fuzzel.package;
in
{
  programs.fuzzel.enable = true;
  # FIXME: dark-mode support (manual color palette specification)
  programs.fuzzel.settings = {
    main = {
      font = "${font.monospace.name}:size=10";
      use-bold = true;
      terminal = lib.mkDefault "foot";
      layer = "overlay";
    };
  };
  wayland.windowManager.sway.config.menu = lib.getExe pkgs.fuzzel;
  home.packages = [ pkgs.fuzzel ];
}
