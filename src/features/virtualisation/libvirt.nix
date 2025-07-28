{ lib, ... }:
{
  dotfield.modules."virtualisation/libvirt".nixos =
    { config, ... }:
    lib.mkMerge [
      {
        virtualisation.libvirtd.enable = true;
        networking.firewall.checkReversePath = "loose";
      }
    ]
    ++ (config.lib.generateSudoersExtraGroupsModules [
      "libvirtd"
      "qemu-libvirtd"
    ]);

  dotfield.modules.workstation.nixos =
    { config, pkgs, ... }:
    lib.mkIf config.virtualisation.libvirtd.enable {
      environment.systemPackages = with pkgs; [ virt-manager ];
      programs.dconf.enable = true;
    };
}
