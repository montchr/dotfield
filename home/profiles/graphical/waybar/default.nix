{
  flake,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.waybar;
in
{
  stylix.targets.waybar.addCss = false;

  programs.waybar.enable = true;
  programs.waybar.package = flake.perSystem.inputs'.nixos-unstable.legacyPackages.waybar;
  programs.waybar.systemd.enable = true;
  programs.waybar.style = ''
    @import "./custom.css";
  '';

  wayland.windowManager.sway.config.bars = [
    {
      command = lib.getExe cfg.package;
    }
  ];
}
