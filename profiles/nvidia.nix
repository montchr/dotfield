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
  nixpkgs.config.allowUnfree = lib.mkForce true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

  hardware.nvidia.package = nvLatest;
  services.xserver.videoDrivers = ["nvidia"];

  # Required for Wayland?
  hardware.nvidia.modesetting.enable = true;

  # Prevent display corruption upon wake from a suspended or hibernated state.
  hardware.nvidia.powerManagement.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = with pkgs; [vaapiVdpau];
}
