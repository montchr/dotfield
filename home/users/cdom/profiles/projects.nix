{
  config,
  lib,
  pkgs,
  ...
}: let
  projectsPath = config.xdg.userDirs.extraConfig."XDG_PROJECTS_DIR";
  contribPath = projectsPath + "/contrib";
in {
  services.git-sync = {
    enable = true;
    repositories = {};
  };
}
