{ config, lib, pkgs, ... }:

{
  boot.kernelModules = ["kvm-amd"];
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
