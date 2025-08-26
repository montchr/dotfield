{
  dotfield.aspects.wayland-wm.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.brightnessctl
        pkgs.kanshi
        # XXX: failing build
        # pkgs.nwg-displays # output management gui
        pkgs.wluma # sensor-adaptive brightness adjustment daemon
      ];
    };
}
