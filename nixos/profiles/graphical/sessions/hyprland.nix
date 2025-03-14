{ pkgs, ... }:
{
  imports = [ ./__wlroots.nix ];

  programs.hyprland.enable = true;
  programs.hyprland.xwayland.enable = true;
  programs.hyprland.withUWSM = true;
}
