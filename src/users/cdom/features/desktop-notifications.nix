{ lib, ... }:
{
  users.cdom.aspects.desktop-sessions__wayland-wm.home =
    { pkgs, ... }:
    {
      services.dunst = {
        # package = flake.perSystem.inputs'.nixpkgs-wayland.packages.dunst;
        settings = {
          global = {
            enable_posix_regex = true;
            notification_limit = 15;
            follow = "mouse";
            width = "300";
            height = "(48, 100)";
            offset = "24";
            origin = "top-right";
            progress_bar = true;
            progress_bar_height = 10;
            progress_bar_min_width = 150;
            progress_bar_max_width = 300;
            progress_bar_corner_radius = 8;
            icon_corner_radius = 4;
            corner_radius = 8;
            dmenu = "${lib.getExe pkgs.fuzzel} --dmenu";
            idle_threshold = "10m";
          };
          urgency_normal = {
            timeout = 10;
          };
        };
      };
    };
}
