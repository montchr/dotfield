# FIXME: make this work -- last i remember, the nvidia driver failed

{config, lib, pkgs, ...}:
{
  # TODO: pluck latest nvidia drivers in override
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # services.xserver.displayManager.gdm.nvidiaWayland = config.programs.xwayland.enable;

  # Be careful...
  # videoDrivers = [ "nvidia" ];
}
