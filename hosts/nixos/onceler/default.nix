{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: let
  inherit (config) my;
in {
  # Note that these are system-level suites, not hm suites.
  imports = with suites;
    gui
    ++ personal
    ++ (with profiles; []);

  my.username = "xtallos";

  home-manager.users.xtallos = {
    config,
    suites,
    ...
  }: {
    imports = with suites; [hmUsers.xtallos] ++ gui;
  };

  nix.buildCores = 0;
}
