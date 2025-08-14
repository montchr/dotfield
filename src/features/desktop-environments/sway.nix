{ self, lib, ... }:
{
  dotfield.features."desktop-environments/sway".nixos =
    { config, pkgs, ... }:
    let
      inherit (lib) optional;
      inherit (config.dotfield.features) hasNvidia;
    in
    {
      imports = [
        self.dotfield.nixos.wayland
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
