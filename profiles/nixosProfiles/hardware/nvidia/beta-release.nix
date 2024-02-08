{
  lib,
  config,
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
  imports = [./common.nix];
  hardware.nvidia.package = nvLatest;
}
