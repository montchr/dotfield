flake@{ ... }:
{
  dotfield.features.wayland-wm.home =
    { config, pkgs, ... }:
    {
      imports = [
        flake.config.dotfield.features.waybar.home

        ./__primary.nix
      ];

      programs.waybar.style = ''
        @import "./custom.css";
      '';
    };

  dotfield.features.theme.home = {
    stylix.targets.waybar.addCss = false;
  };
}
