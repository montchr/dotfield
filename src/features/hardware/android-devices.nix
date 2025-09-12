{
  aspects.workstation.nixos =
    { config, pkgs, ... }:
    {
      programs.adb.enable = true;

      environment.systemPackages = [
        pkgs.android-file-transfer # => <https://github.com/whoozle/android-file-transfer-linux>
        pkgs.android-udev-rules
      ];

      users.groups.adbusers = { inherit (config.users.groups.wheel) members; };
    };
}
