{
  dotfield.features.wayland-wm.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.grim
        pkgs.kooha
        pkgs.satty
        pkgs.slurp
        pkgs.wf-recorder
      ];
    };
}
