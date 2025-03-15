{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) optional;
  inherit (config.dotfield.features) hasNvidia;
in
{
  imports = [
    ./_wayland-wm.nix
  ];

  xdg.portal.wlr.enable = true;

  programs.uwsm.waylandCompositors.sway = {
    prettyName = "Sway";
    comment = "Sway compositor managed by UWSM";
    binPath = "/run/current-system/sw/bin/sway";
  };

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    xwayland.enable = true;
    extraOptions = optional hasNvidia "--unsupported-gpu";
  };

  environment.etc."sway/config".text = ''
    exec sleep 5; systemctl --user start kanshi.service
  '';

  environment.systemPackages = with pkgs; [
    sway
  ];
}
