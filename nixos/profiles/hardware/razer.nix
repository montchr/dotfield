# Razer Deathadder V3 Pro Wireless Mouse
# <https://openrazer.github.io/>
#   NOTE: admin users should be added to `hardware.openrazer.users`!
{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.razergenie # Razer device configuration GUI (Qt)
    # pkgs.polychromatic # Lighting management GUI for Razer devices
  ];

  hardware.openrazer = {
    enable = true;
    keyStatistics = false;
    # Notifications are super frequent, repetitive, and sometimes just report 0%.
    # The upstream issue tracker has quite a few related issues:
    # <https://github.com/openrazer/openrazer/issues?q=notification+battery>
    mouseBatteryNotifier = false;
  };
}
