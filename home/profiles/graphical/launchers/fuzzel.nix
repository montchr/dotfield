{ pkgs, lib, ... }:
{
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      use-bold = true;
      terminal = lib.mkDefault "foot";
      layer = "overlay";
    };
  };
  wayland.windowManager.sway.config.menu = lib.getExe pkgs.fuzzel;
  home.packages = [ pkgs.fuzzel ];
}
