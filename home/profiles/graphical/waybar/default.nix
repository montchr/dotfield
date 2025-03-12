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
  imports = [ ./__primary.nix ];

  stylix.targets.waybar.addCss = false;

  programs.waybar.enable = true;
  programs.waybar.package = flake.perSystem.inputs'.nixos-unstable.legacyPackages.waybar;
  # FIXME: still results in duplicate bars upon config reload
  #  programs.waybar.systemd.enable = true;
  programs.waybar.style = ''
    @import "./custom.css";
  '';

  home.packages = [ cfg.package ];
}
