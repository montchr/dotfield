{
  flake,
  pkgs,
  config,
  ...
}:
let
  theme = config.theme;
in
{
  services.mako = {
    enable = true;
    package = flake.perSystem.inputs'.nixpkgs-wayland.packages.mako;
    anchor = "top-center";
    layer = "overlay";
    defaultTimeout = 12000;
    padding = "4,8";
    width = 300;
    height = 60;
    borderSize = 2;
    extraConfig = ''
      max-history=50
    '';
  };
}
