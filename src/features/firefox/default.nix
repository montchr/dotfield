{
  aspects.graphical.nixos =
    { pkgs, ... }:
    {
      programs.firefox = {
        enable = true;
        nativeMessagingHosts.packages = [
          pkgs.tridactyl-native
          pkgs.passff-host
        ];
      };
    };

  aspects.graphical.home =
    hmArgs@{ pkgs, ... }:
    {
      programs.firefox = {
        enable = true;
        package =
          if (hmArgs.osConfig.programs.firefox.enable or false) then
            (hmArgs.osConfig.programs.firefox.package or pkgs.firefox)
          else
            pkgs.firefox;
      };
    };

  aspects.desktop-sessions__gnome.nixos = {
    dconf.settings."org/gnome/desktop/notifications/application/firefox" = {
      application-id = "firefox.desktop";
    };
  };
}
