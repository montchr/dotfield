{ flake, pkgs, ... }:
{
  services.dunst = {
    enable = true;
    package = flake.perSystem.inputs'.nixpkgs-wayland.packages.dunst;
    iconTheme.name = "Adwaita";
    iconTheme.package = pkgs.adwaita-icon-theme;
    iconTheme.size = "16x16";
    settings = {
      global = {
        width = 400;
        height = 60;
        offset = "30x50";
        origin = "top-center";
        transparency = 10;
      };
      urgency_normal = {
        timeout = 10;
      };
    };
  };
}
