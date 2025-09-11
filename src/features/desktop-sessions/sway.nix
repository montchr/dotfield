flake@{ ... }:
{
  aspects.desktop-sessions__sway = {
    requires = [ "desktop-sessions__wayland-wm" ];

    nixos =
      { pkgs, ... }:
      {
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

    home =
      { pkgs, config, ... }:
      let
        prefs = flake.config.meta.users.${config.home.username}.preferences;
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
            # FIXME: this does not help
            # <https://wiki.nixos.org/wiki/Sway#GTK_apps_take_an_exceptionally_long_time_to_start>
            include /etc/sway/config.d/*
          '';
          config = {
            menu = prefs.wayland.menu;
            terminal = app (prefs.term);
            bars = [
              { command = app prefs.wayland.bar; }
            ];
          };
        };
      };
  };
}
