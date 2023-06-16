{...}: {
  imports = [
    # ./profiles/sops.nix
    ./users
  ];

  # fileSystems."/nix" = {
  #   device = "/dev/disk/by-id/scsi-0HC_Volume_19315958";
  #   fsType = "ext4";
  #   neededForBoot = true;
  #   options = ["noatime"];
  # };

  system.stateVersion = "23.05";
}
