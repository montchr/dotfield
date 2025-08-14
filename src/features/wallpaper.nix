{
  dotfield.features."desktop-environments/wayland-wm".home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.swaybg
        pkgs.waypaper
      ];
    };
}
