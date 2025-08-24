{
  dotfield.aspects.graphical.nixos = {
    programs.firefox.enable = true;
  };

  dotfield.aspects.workstation.nixos =
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

  dotfield.aspects.graphical.home =
    homeArgs@{ pkgs, ... }:
    {
      programs.firefox = {
        enable = true;
        package =
          if (homeArgs.osConfig.programs.firefox.enable or false) then
            (homeArgs.osConfig.programs.firefox.package or pkgs.firefox)
          else
            pkgs.firefox;
      };
    };

  dotfield.aspects.workstation.home =
    { config, ... }:
    {
      dconf.settings."org/gnome/desktop/notifications/application/firefox" = {
        application-id = "firefox.desktop";
      };
    };
}
