### iwd

# - is a "modern replacement" for wpa_supplicant
# - provides WPA3 support, which wpa_supplicant does not
# - is developed by Intel

#### eduroam:

# - <https://wiki.archlinux.org/title/Iwd#EAP-PEAP>
# - <https://wiki.archlinux.org/title/Iwd#eduroam>
{
  dotfield.features.networking__iwd.nixos = {
    networking.wireless.iwd = {
      enable = true;
      settings = {
        # Support saving encrypted passwords
        # <https://wiki.archlinux.org/title/Iwd#Network_configuration>
        # <https://wiki.archlinux.org/title/Iwd#EAP-PEAP> => $ iconv -t utf16le | openssl md4 -provider legacy
        General.EnableNetworkConfiguration = true;

        IPv6.Enabled = true;
        Settings.AutoConnect = true;
      };
    };

    networking.networkmanager.wifi.backend = "iwd";
    services.connman.wifi.backend = "iwd";
  };
}
