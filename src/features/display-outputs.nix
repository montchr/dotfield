{
  dotfield.modules."desktop-environments/wayland-wm".nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.brightnessctl
        pkgs.kanshi
        pkgs.nwg-displays # output management gui
        pkgs.wluma # sensor-adaptive brightness adjustment daemon
      ];
    };
}
