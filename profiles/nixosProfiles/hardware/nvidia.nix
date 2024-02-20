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
  # At least until NVIDIA fully releases a free driver...
  # NOTE: This requires `pkgs` to be configured with `allowUnfree = true`.
  hardware.enableRedistributableFirmware = true;

  hardware.nvidia.package = nvLatest;
  services.xserver.videoDrivers = ["nvidia"];

    hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;

  # TODO: remind self about nature of vaapiVdpau -- this may very likely relate
  # to issues with Steam Remote Play encountered whilst trying to run Baldur's
  # Gate 3 on ryosuke as client machine (host was DORE (mswin), which is the
  # same machine as boschic, and thus nvidia)...
extraPackages = with pkgs; [vaapiVdpau];

  };

  # Required for Wayland support. I thought this setting only related
  # to switching between integrated/discrete GPUs, but apparently not?
  # TODO: provide more info about why this is necessary, incl. sources
  hardware.nvidia.modesetting.enable = true;

  # Prevent display corruption upon wake from a suspended or hibernated state.
  # Probably does other important things too. We don't want the GPU to break...
  # hardware.nvidia.powerManagement.enable = true;
}
