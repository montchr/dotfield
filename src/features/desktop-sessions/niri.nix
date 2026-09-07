{ inputs, moduleWithSystem, ... }:
{
  aspects.desktop-sessions__niri = {
    requires = [ "desktop-sessions__wayland-wm" ];

    nixos = moduleWithSystem (
      perSystem@{ inputs' }:
      nixos@{ pkgs, ... }:
      {
        programs.niri = {
          enable = true;
          package = pkgs.niri;
        };

        xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gnome ];

        xdg.portal.config =
          let
            common = {
              default = [
                "gnome"
                "gtk"
              ];
              "org.freedesktop.impl.portal.Secret" = [ "gnome-keyring" ];
            };
          in
          {
            inherit common;
            # <https://yalter.github.io/niri/Important-Software.html#portals>
            niri = common;
          };
      }
    );

    home = moduleWithSystem (
      perSystem@{ config, ... }:
      nixos@{ pkgs, ... }:
      {
        home.packages = [
          config.packages.niri-sidebar
          pkgs.nirius # https://git.sr.ht/~tsdh/nirius
          pkgs.nwg-displays
        ];

        services.swayidle.timeouts = [
          {
            timeout = 20 * 60;
            command = "niri msg output * off";
            resumeCommand = "niri msg output * on";
          }
        ];
      }
    );
  };
}
