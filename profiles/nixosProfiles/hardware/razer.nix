##: <https://openrazer.github.io/>
#   NOTE: admin users should be added to `hardware.openrazer.users`!
{config,...}: {
  hardware.openrazer = {
    enable = true;
    keyStatistics = false;
    mouseBatteryNotifier = true;
  };
}
