{
  pkgs,
  ops,
  ...
}: let
  inherit (ops.metadata) hosts;
in {
  imports = [
    ./filesystems.nix
    ./network.nix
    ./profiles/sops.nix
    ./users
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  environment.systemPackages = [
    pkgs.borgbackup
  ];

  users.users.root.openssh.authorizedKeys.keys =
    hosts.boschic.users.seadoom.keys
    ++ hosts.tuvix.users.cdom.keys
    ++ hosts.ryosuke.users.cdom.keys;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "23.05"; # Did you read the comment?
}
