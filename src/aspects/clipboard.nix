{ moduleWithSystem, ... }:
{
  dotfield.aspects.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.cliphist
        pkgs.wl-clipboard
      ];
    };

  dotfield.aspects.graphical.home = moduleWithSystem (
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
