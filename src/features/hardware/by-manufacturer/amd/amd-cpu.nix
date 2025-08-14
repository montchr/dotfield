{ lib, ... }:
{
  dotfield.features."hardware/amd/cpu".nixos =
    { config, ... }:
    {
      boot.kernelModules = lib.optional config.virtualisation.libvirtd.enable "kvm-amd";
      hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    };
}
