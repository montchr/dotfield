{ config, lib, ... }:
let
  inherit (lib) mkDefault optional;
in
{
  boot.kernelModules = optional config.virtualisation.libvirtd.enable "kvm-amd";
  hardware.cpu.amd.updateMicrocode = mkDefault config.hardware.enableRedistributableFirmware;
}
