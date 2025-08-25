{ lib, ... }:
{
  dotfield.aspects.libvirt.nixos =
    { config, ... }:
    {
      users.groups.libvirtd.members = config.users.groups.wheel.members;
      users.groups.qemu-libvirtd.members = config.users.groups.wheel.members;

      virtualisation.libvirtd.enable = true;
      networking.firewall.checkReversePath = "loose";
    };

  dotfield.aspects.workstation.nixos =
    { config, pkgs, ... }:
    lib.mkIf config.virtualisation.libvirtd.enable {
      environment.systemPackages = with pkgs; [ virt-manager ];
      programs.dconf.enable = true;
    };
}
