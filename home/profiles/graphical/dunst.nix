{ flake, pkgs, ... }:
{
  services.dunst = {
    enable = true;
    package = flake.perSystem.inputs'.nixpkgs-wayland.packages.dunst;
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
