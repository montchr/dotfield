{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dosfstools
    gptfdisk
    iputils
    usbutils
    utillinux
  ];
}
