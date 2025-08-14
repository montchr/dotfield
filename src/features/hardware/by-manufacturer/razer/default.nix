# Razer Deathadder V3 Pro Wireless Mouse
# <https://openrazer.github.io/>
{ lib, ... }:
{
  dotfield.features.workstation.nixos =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        environment.systemPackages = [
          pkgs.razergenie # Razer device configuration GUI (Qt)
          # pkgs.polychromatic # Lighting management GUI for Razer devices
        ];

        hardware.openrazer = {
          enable = true;
          users = config.users.groups.wheel.members;
          keyStatistics = false;
          # Notifications are super frequent, repetitive, and sometimes just report 0%.
          # The upstream issue tracker has quite a few related issues:
          # <https://github.com/openrazer/openrazer/issues?q=notification+battery>
          mouseBatteryNotifier = false;
        };
      }
    ]
    ++ (config.lib.generateSudoersExtraGroupsModules [ "openrazer" ]);
}
