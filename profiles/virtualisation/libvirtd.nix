{
  config,
  lib,
  pkgs,
  ...
}: {
  virtualisation.libvirtd.enable = true;
  networking.firewall.checkReversePath = false;
  environment.systemPackages = with pkgs; [
    virt-manager
  ];
  # boot.zfs.devNodes = "/dev";
}
