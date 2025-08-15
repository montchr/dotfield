{
  dotfield.features."hardware/android-devices".nixos =
    { config, pkgs, ... }:
    {
      programs.adb.enable = true;
      environment.systemPackages = [
        pkgs.android-file-transfer # => <https://github.com/whoozle/android-file-transfer-linux>
      ];
      users.groups.adbusers.members = config.users.groups.wheel.members;
    };
}
