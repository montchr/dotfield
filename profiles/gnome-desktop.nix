{config, lib, pkgs, ...}:
{
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;
  services.xserver.displayManager.gdm.autoSuspend = false;
  services.xserver.desktopManager.gnome.enable = true;

  services.gnome.chrome-gnome-shell.enable = true;

  # TODO: necessary? consider disabling until needed
  programs.xwayland.enable = true;

  xdg.portal.enable = true;
  xdg.portal.gtkUsePortal = true;
}
