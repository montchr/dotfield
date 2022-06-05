{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;
  services.xserver.desktopManager.gnome.enable = true;

  # FIXME: does absolutely nothing?! autosuspend still happens...
  services.xserver.displayManager.gdm.autoSuspend = false;

  # Required for Firefox integration in home-manager
  services.gnome.chrome-gnome-shell.enable = true;

  # A file previewer for the GNOME desktop environment.
  # https://gitlab.gnome.org/GNOME/sushi
  services.gnome.sushi.enable = true;

  xdg.portal.enable = true;
  xdg.portal.gtkUsePortal = true;

  programs.gnupg.agent.pinentryFlavor = "gnome3";

  environment.systemPackages = with pkgs; [
    gnome.gnome-tweaks
  ];
}
