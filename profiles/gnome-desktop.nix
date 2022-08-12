{
  config,
  lib,
  pkgs,
  profiles,
  ...
}: {
  imports = with profiles; [login.gdm];

  services.xserver.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Required for Firefox integration in home-manager
  services.gnome.chrome-gnome-shell.enable = true;

  services.gnome.sushi.enable = true;
  programs.gnupg.agent.pinentryFlavor = "gnome3";
}
