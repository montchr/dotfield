{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.boot.kernelPackages) nvidiaPackages;

  nvStable = nvidiaPackages.stable;
  nvBeta = nvidiaPackages.beta;
  nvLatest =
    if (lib.versionOlder nvBeta.version nvStable.version)
    then nvStable
    else nvBeta;
in {
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  hardware.nvidia.package = nvLatest;
  services.xserver.videoDrivers = ["nvidia"];
  hardware.opengl.extraPackages = with pkgs; [vaapiVdpau];
  # Seems required for Wayland, though I thought this setting only applied to
  # GPU switching which isn't relevant to any of my hardware configurations.
  hardware.nvidia.modesetting.enable = true;
  # Prevent display corruption upon wake from a suspended or hibernated state.
  hardware.nvidia.powerManagement.enable = true;
}
