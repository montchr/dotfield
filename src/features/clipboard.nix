{ moduleWithSystem, ... }:
{
  dotfield.modules.graphical.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.cliphist
        pkgs.wl-clipboard
      ];
    };

  dotfield.modules.graphical.home = moduleWithSystem (
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
