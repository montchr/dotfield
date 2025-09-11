{
  aspects.hardware__amd__cpu.nixos =
    { config, lib, ... }:
    {
      boot.kernelModules = lib.optional config.virtualisation.libvirtd.enable "kvm-amd";
      hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    };
}
