{ self, ... }:
{
  dotfield.features."desktop-environments/wayland-wm".home =
    { config, pkgs, ... }:
    {
      imports = [
        self.dotfield.features.waybar.home

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
