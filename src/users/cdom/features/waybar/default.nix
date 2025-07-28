{ self, ... }:
{
  dotfield.modules."desktop-environments/wayland-wm".home =
    { config, pkgs, ... }:
    {
      imports = [
        self.dotfield.modules.waybar.home

        ./__primary.nix
      ];

      programs.waybar.style = ''
        @import "./custom.css";
      '';
    };

  dotfield.modules.theme.home = {
    stylix.targets.waybar.addCss = false;
  };
}
