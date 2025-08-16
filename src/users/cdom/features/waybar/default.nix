flake@{ ... }:
{
  dotfield.users.cdom.features.wayland-wm.home =
    { config, pkgs, ... }:
    {
      imports = [
        flake.config.dotfield.users.cdom.features.waybar.home

        ./__primary.nix
      ];

      programs.waybar.style = ''
        @import "./custom.css";
      '';
    };

  dotfield.users.cdom.features.theme.home = {
    stylix.targets.waybar.addCss = false;
  };
}
