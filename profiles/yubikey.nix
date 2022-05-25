{ config, lib, pkgs, ... }:

{
  services.pcscd.enable = true;
  services.udev.packages = with pkgs; [ yubikey-personalization ];
}
