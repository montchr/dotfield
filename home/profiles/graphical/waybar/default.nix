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
  # FIXME: breaks custom css -- generate colors and families manually
  # stylix.targets.waybar.enable = false;
  stylix.targets.waybar.addCss = false;

  programs.waybar.enable = true;
  programs.waybar.package = flake.perSystem.inputs'.nixos-unstable.legacyPackages.waybar;
  programs.waybar.systemd.enable = true;
  programs.waybar.style = lib.mkAfter (builtins.readFile ./style.css);

  wayland.windowManager.sway.config.bars = [
    {
      command = lib.getExe cfg.package;
    }
  ];
}
