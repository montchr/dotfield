{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  # Note that these are system-level suites, not hm suites.
  imports = with suites;
    gui
    ++ personal
    ++ (with profiles; [
      users.xtallos
    ]);

  home-manager.users.xtallos = {suites, ...}: {
    imports = [hmUsers.xtallos] ++ suites.gui;
  };
}
