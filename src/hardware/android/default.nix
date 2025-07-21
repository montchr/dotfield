{
  flake.modules.nixos.workstation =
    { pkgs, ... }:
    {
      programs.adb.enable = true;

      environment.systemPackages = [
        pkgs.android-file-transfer # => <https://github.com/whoozle/android-file-transfer-linux>
      ];
    };

  flake.modules.nixos.admin-user = {
    dotfield.guardian.extraGroups = [ "adbusers" ];
  };
}
