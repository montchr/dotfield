flake@{ lib, ... }:
{
  aspects.desktop-sessions__sway = {
    requires = [ "desktop-sessions__wayland-wm" ];

    nixos =
      { pkgs, ... }:
      {
        xdg.portal.wlr.enable = true;

        programs.sway = {
          enable = true;
          wrapperFeatures.gtk = true;
          xwayland.enable = true;
        };

        environment.systemPackages = with pkgs; [
          sway
        ];
      };

    home =
      { pkgs, config, ... }:
      let
        app = cmd: "${app'} ${cmd}";
        app' = "${pkgs.uwsm}/bin/uwsm-app --";
      in
      {
        wayland.windowManager.sway = {
          enable = true;
          # A `null` value tells home-manager to use the package from the
          # system level.
          package = null;
          extraConfigEarly = ''
            include /etc/sway/config.d/*
          '';
          config = {
            menu = lib.mkDefault "fuzzel";
            terminal = lib.mkDefault (app "ghostty");
          };
        };
      };
  };
}
