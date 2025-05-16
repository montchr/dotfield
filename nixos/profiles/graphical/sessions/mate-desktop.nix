{
  flake,
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../common.nix ];

  services.xserver.enable = true;
  services.xserver.desktopManager.mate.enable = true;
  services.xserver.desktopManager.mate.enableWaylandSession = true;

  # home-manager.sharedModules = lib.singleton {
  #   imports = [ "${flake.self}/home/profiles/graphical/sessions/gnome/common.nix" ];
  # };

  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;
}
