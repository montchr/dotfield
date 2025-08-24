flake@{ ... }:
{
  dotfield.aspects.hyprland.nixos = {
    programs.hyprland.withUWSM = true;
    programs.uwsm.waylandCompositors.hyprland = {
      prettyName = "Hyprland";
      comment = "Hyprland compositor managed by UWSM";
      binPath = "/run/current-system/sw/bin/Hyprland";
    };

    programs.hyprland.enable = true;
    programs.hyprland.xwayland.enable = true;
  };

  dotfield.aspects.hyprland.home = {
    imports = [
      flake.config.dotfield.aspects.wayland-wm.home
    ];
  };
}
