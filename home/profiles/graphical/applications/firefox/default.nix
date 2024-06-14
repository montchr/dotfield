hmArgs@{ pkgs, ... }:
{
  imports = [ ./profiles.nix ];

  programs.firefox = {
    enable = true;
    package =
      if (hmArgs.osConfig.programs.firefox.enable or false) then
        (hmArgs.osConfig.programs.firefox.package or pkgs.firefox)
      else
        pkgs.firefox;
  };

  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "firefox.desktop" ];
    "text/xml" = [ "firefox.desktop" ];
    "x-scheme-handler/http" = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };
}
