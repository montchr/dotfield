{ lib, ... }:
{
  dotfield.modules.workstation.nixos =
    { config, pkgs, ... }:
    (
      lib.mkMerge [
        {
          programs.adb.enable = true;
          environment.systemPackages = [
            pkgs.android-file-transfer # => <https://github.com/whoozle/android-file-transfer-linux>
          ];
        }
      ]
      ++ (config.lib.generateSudoersExtraGroupsModules [ "adbusers" ])
    );
}
