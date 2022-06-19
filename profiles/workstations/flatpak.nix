{ config, lib, pkgs, ... }:

{
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  xdg.portal.gtkUsePortal = true;
}
