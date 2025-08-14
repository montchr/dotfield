{ moduleWithSystem, ... }:
{
  dotfield.features.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.cliphist
        pkgs.wl-clipboard
      ];
    };

  dotfield.features.graphical.home = moduleWithSystem (
    perSystem@{ config }:
    { pkgs, ... }:
    {
      services.cliphist.enable = true;
      home.packages = [
        perSystem.config.packages.clipsel
      ];
    }
  );
}
