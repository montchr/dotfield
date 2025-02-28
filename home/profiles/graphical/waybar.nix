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
  programs.waybar.enable = true;
  programs.waybar.package = flake.perSystem.inputs'.nixos-unstable.legacyPackages.waybar;
  programs.waybar.systemd.enable = true;
  wayland.windowManager.sway.config.bars = [
    {
      command = lib.getExe cfg.package;
    }
  ];
}
