flake@{ ... }:
{
  dotfield.users.cdom.aspects.wayland-wm.home =
    { config, pkgs, ... }:
    {
      imports = [
        flake.config.dotfield.users.cdom.aspects.waybar.home

        ./__primary.nix
      ];

      programs.waybar.style = ''
        @import "./custom.css";
      '';
    };

  dotfield.users.cdom.aspects.theme.home = {
    stylix.targets.waybar.addCss = false;
  };
}
