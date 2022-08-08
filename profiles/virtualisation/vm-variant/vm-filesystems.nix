{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.nixos-vm;
  systemName = config.system.name;
  mkVMDefault = lib.mkOverride 900;
in {
  virtualisation = {
    # https://wiki.qemu.org/Documentation/9psetup#Performance_Considerations
    # FIXME: currently 500K seems to be the limit?
    msize = mkVMDefault 104857600; # 100M

    graphics = mkVMDefault false;

    diskImage = mkVMDefault "${cfg.dataHome}/${systemName}-rootfs.qcow2";
    # emptyDiskImages = lib.mkDefault [4096];

    sharedDirectories = {
      shared = {
        source = mkVMDefault "${cfg.dataHome}/shared";
        target = mkVMDefault "/mnt/shared";
      };
      #cfg.mounts.extraMounts
      # (lib.mkIf cfg.mounts.mountHome {
      #   # FIXME: requires impure eval
      #   # home = config.users.users.${user}.home or "/home/${user}";
      # })
    };
  };
}
