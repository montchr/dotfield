{lib, ...}: {
  imports = [
    ./boot.nix
    ./filesystems.nix
    # ./network.nix
    ./profiles/sops.nix
    ./users/cdom.nix
  ];

  disko.devices = import ./disk-config.nix {inherit lib;};

  # networking.useDHCP = true;

  # environment.systemPackages = with pkgs; [
  #   tor
  #   borgbackup
  # ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "23.05"; # Did you read the comment?
}
