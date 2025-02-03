{ lib, config, ... }:
let
  cfg = config.programs.waybar;
in
{
  programs.waybar.enable = true;
  # programs.waybar.systemd.enable = true;
  wayland.windowManager.sway.config.bars = [
    {
      command = lib.getExe cfg.package;
    }
  ];
}
