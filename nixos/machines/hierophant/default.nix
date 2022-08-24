{...}: {
  imports = [
    ./profiles/sops.nix
    ./users
  ];

  # FIXME: this host cannot be "trusted"...
  networking.firewall.enable = false;
  networking.firewall.trustedInterfaces = ["enp1s0"];

  fileSystems."/nix" = {
    device = "/dev/disk/by-id/scsi-0HC_Volume_19315958";
    fsType = "ext4";
    neededForBoot = true;
    options = ["noatime"];
  };

  system.stateVersion = "21.11";
}
