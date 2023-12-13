{
  config,
  lib,
  pkgs,
  ...
}: let
  # TODO: why not use `pkgs.linuxPackages_latest.nvidiaPackages` directly?
  # perhaps that would help avoid using unstable kernel packages
  inherit (config.boot.kernelPackages) nvidiaPackages;

  nvStable = nvidiaPackages.stable;
  nvBeta = nvidiaPackages.beta;
  nvLatest =
    if (lib.versionOlder nvBeta.version nvStable.version)
    then nvStable
    else nvBeta;
in {
  # At least until NVIDIA fully releases a free driver...
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  # FIXME: this is a kludge -- we only need the latest nvidia driver -- is it
  # really necessary to force the latest version of ALL kernel packages? because
  # doing so can lead to incompatibilities with the most recent upstream updates
  # -- ESPECIALLY when using `nixos-unstable`, which we do want most of the
  # time, but maybe not for kernel packages...
  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.nvidia.package = nvLatest;
  services.xserver.videoDrivers = ["nvidia"];

  # TODO: remind self about nature of vaapiVdpau -- this may very likely relate
  # to issues with Steam Remote Play encountered whilst trying to run Baldur's
  # Gate 3 on ryosuke as client machine (host was DORE (mswin), which is the
  # same machine as boschic, and thus nvidia)...
  hardware.opengl.extraPackages = with pkgs; [vaapiVdpau];

  # Required for Wayland support. I thought this setting only related
  # to switching between integrated/discrete GPUs, but apparently not?
  # TODO: provide more info about why this is necessary, incl. sources
  hardware.nvidia.modesetting.enable = true;

  # Prevent display corruption upon wake from a suspended or hibernated state.
  # Probably does other important things too. We don't want the GPU to break...
  hardware.nvidia.powerManagement.enable = true;
}
