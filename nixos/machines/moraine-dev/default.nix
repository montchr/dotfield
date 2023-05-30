{lib, ...}: {
  imports = [
    # ./filesystems.nix
    # ./profiles/sops.nix
    # ./users
  ];

  # disko.devices = import ./disk-config.nix {inherit lib;};

  boot.loader.grub = {
    devices = ["/dev/sda"];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  users.users.root.openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMmT25KQRhB56Ym0w81lzRbcYNqWffihEsq/RZ2QE754 cdom@tuvix"];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "23.05"; # Did you read the comment?
}
