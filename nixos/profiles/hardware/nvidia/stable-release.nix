{config, ...}: {
  imports = [./common.nix];

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
}
