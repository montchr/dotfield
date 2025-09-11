{ moduleWithSystem, ... }:
{
  aspects.desktop-sessions__niri = {
    requires = [ "desktop-sessions__wayland-wm" ];

    nixos = moduleWithSystem (
      perSystem@{ inputs' }:
      nixos@{ pkgs, ... }:
      {
        programs.niri = {
          enable = true;
          package = perSystem.inputs'.niri.packages.default;
        };

        programs.uwsm.waylandCompositors.niri = {
          prettyName = "Niri";
          comment = "Niri compositor managed by UWSM";
          binPath = "/run/current-system/sw/bin/niri";
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
  };
}
