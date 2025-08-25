flake@{ ... }:
{
  dotfield.aspects.hyprland.nixos = {
    imports = [
      flake.config.dotfield.aspects.wayland-wm.nixos
    ];

    programs.hyprland.withUWSM = true;
    programs.uwsm.waylandCompositors.hyprland = {
      prettyName = "Hyprland";
      comment = "Hyprland compositor managed by UWSM";
      binPath = "/run/current-system/sw/bin/Hyprland";
    };

    programs.hyprland.enable = true;
    programs.hyprland.xwayland.enable = true;
  };

  dotfield.aspects.hyprland.home =
    { pkgs, ... }:
    {
      imports = [
        flake.config.dotfield.aspects.wayland-wm.home
      ];

      home.packages = with pkgs; [
        grimblast
        hypridle
        hyprlock
        hyprpaper
        hyprpicker
        hyprpolkitagent
        hyprsunset
      ];

      misc = {
        # No, Hyprland, we absolutely do not want to see your pervy,
        # voyeuristic, and unsolicited anime girl upskirt background.
        # Drew DeVault warned us about you.  Get a fucking life.
        disable_hyprland_logo = true;
      };
    };
}
