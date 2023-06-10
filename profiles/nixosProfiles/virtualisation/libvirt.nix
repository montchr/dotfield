_: {
  virtualisation.libvirtd.enable = true;
  networking.firewall.checkReversePath = "loose";
  dotfield.guardian.user.extraGroups = [
    "libvirtd"
    # https://discourse.nixos.org/t/set-up-vagrant-with-libvirt-qemu-kvm-on-nixos/14653
    "qemu-libvirtd"
  ];
}
