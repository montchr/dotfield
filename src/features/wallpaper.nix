{
  dotfield.features.wayland-wm.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.swaybg
        pkgs.waypaper
      ];
    };
}
