{
  aspects.workstation.nixos =
    { pkgs, ... }:
    {
      programs.adb.enable = true;

      dotfield.guardian.extraGroups = [ "adbusers" ];

      environment.systemPackages = [
        pkgs.android-file-transfer # => <https://github.com/whoozle/android-file-transfer-linux>
        pkgs.android-udev-rules
      ];
    };
}
