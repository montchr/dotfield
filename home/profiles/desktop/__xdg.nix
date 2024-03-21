{ config, ... }:
let
  inherit (config.home) homeDirectory;
  inherit (config.accounts.email) maildirBasePath;
in
{
  # TODO
  # xdg.desktopEntries = ...

  xdg = {
    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        # TODO: somehow share this value with home-manager git-sync?
        XDG_PROJECTS_DIR = homeDirectory + "/Developer";
        XDG_MAIL_DIR = "${homeDirectory}/${maildirBasePath}";
      };
    };

    mimeApps.enable = true;
  };
}
