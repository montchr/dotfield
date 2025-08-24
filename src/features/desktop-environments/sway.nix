flake@{ lib, ... }:
{
  dotfield.aspects.sway.nixos =
    { config, pkgs, ... }:
    {
      imports = [
        flake.config.dotfield.aspects.wayland-wm.nixos
      ];

      xdg.portal.wlr.enable = true;

      programs.uwsm.waylandCompositors.sway = {
        prettyName = "Sway";
        comment = "Sway compositor managed by UWSM";
        binPath = "/run/current-system/sw/bin/sway";
      };

      programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        xwayland.enable = true;
      };

      environment.systemPackages = with pkgs; [
        sway
      ];
    };
}
