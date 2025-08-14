{
  dotfield.features.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.xwayland-satellite
      ];
    };
}
