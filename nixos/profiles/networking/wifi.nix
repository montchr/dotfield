# iwd:
# - is basically required for reliable Broadcom network controller support.
# - is a "modern replacement" for wpa_supplicant
# - provides WPA3 support, which wpa_supplicant does not
# - is developed by Intel
{
  networking.wireless.iwd = {
    enable = true;
    settings = {
      General.EnableNetworkConfiguration = true;
      IPv6.Enabled = true;
      Settings.AutoConnect = true;
    };
  };

  networking.networkmanager.wifi.backend = "iwd";
  services.connman.wifi.backend = "iwd";
}
