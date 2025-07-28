{
  dotfield.modules.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.xwayland-satellite
      ];
    };
}
