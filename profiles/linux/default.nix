{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dosfstools
    gptfdisk
    iputils
    usbutils
    utillinux
  ];

  # For rage encryption, all hosts need a ssh key pair
  services.openssh = {
    enable = true;
    openFirewall = lib.mkDefault false;
  };

  # Automatically kill processes when running low on available memory.
  # https://github.com/rfjakob/earlyoom
  services.earlyoom.enable = true;
}
