{ pkgs, ... }:
{
  programs.uwsm.waylandCompositors.niri = {
    prettyName = "Niri";
    comment = "Niri compositor managed by UWSM";
    binPath = "/run/current-system/sw/bin/niri";
  };

  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };
}
