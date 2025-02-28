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
        width = 300;
        height = 60;
        offset = "30x50";
        origin = "top-center";
        transparency = 10;
        frame_color = "#eceff1";
        font = "monospace 8";
      };
      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
    };
  };
}
