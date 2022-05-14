# FIXME: make this work -- last i remember, the nvidia driver failed

{config, lib, pkgs, ...}:
{
  # TODO: pluck latest nvidia drivers in override
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # services.xserver.displayManager.gdm.nvidiaWayland = services.xserver.displayManager.gdm.wayland;

  # Be careful...
  # videoDrivers = [ "nvidia" ];
}
