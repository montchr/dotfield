{ flake, ... }:
{
  nixpkgs.overlays = [ flake.inputs.niri.overlays.niri ];

  programs.uwsm.waylandCompositors.niri = {
    prettyName = "Niri";
    comment = "Niri compositor managed by UWSM";
    binPath = "/run/current-system/sw/bin/niri";
  };

  programs.niri = {
    enable = true;
  };
}
