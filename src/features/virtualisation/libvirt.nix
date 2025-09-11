{
  aspects.libvirt.nixos =
    { config, ... }:
    {
      virtualisation.libvirtd.enable = true;
      networking.firewall.checkReversePath = "loose";
      users.groups.libvirtd = { inherit (config.users.groups.wheel) members; };
      users.groups.qemu-libvirtd = { inherit (config.users.groups.wheel) members; };
    };
}
