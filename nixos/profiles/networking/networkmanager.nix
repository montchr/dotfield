{ lib, ... }:
{
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };

  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;
}
