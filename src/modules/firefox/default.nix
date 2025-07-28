{ self, lib, ... }:
{
  dotfield.modules.firefox.nixos =
    { pkgs, ... }:
    {
      programs.firefox = {
        enable = true;
        # Firefox must be wrapped with native messaging hosts in the system scope.
        nativeMessagingHosts.packages = [
          pkgs.bukubrow
          pkgs.tridactyl-native
          pkgs.passff-host
        ];
      };
    };

  dotfield.modules.firefox.home =
    lib.mkIf self.dotfield.graphical.nixos.services.desktopManager.gnome.enable
      {
        dconf.settings."org/gnome/desktop/notifications/application/firefox" = {
          application-id = "firefox.desktop";
        };
      };
}
