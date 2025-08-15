{
  dotfield.features.hyprland.nixos = {
    programs.hyprland.withUWSM = true;
    programs.uwsm.waylandCompositors.hyprland = {
      prettyName = "Hyprland";
      comment = "Hyprland compositor managed by UWSM";
      binPath = "/run/current-system/sw/bin/Hyprland";
    };

    programs.hyprland.enable = true;
    programs.hyprland.xwayland.enable = true;
  };
}
