{
  dotfield.aspects.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.xwayland-satellite
      ];
    };
}
