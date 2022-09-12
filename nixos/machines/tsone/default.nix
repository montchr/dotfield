{
  config,
  pkgs,
  lib,
  peers,
  ...
}: let
  inherit (peers.hosts.tsone) ipv4 ipv6;
  interface = "eth0";
in {
  imports = [
    ./boot.nix
    ./filesystems.nix
    # ./network.nix
    ./profiles/sops.nix
    ./users/cdom.nix
  ];

  networking.useDHCP = true;

  environment.systemPackages = with pkgs; [
    tor
    borgbackup
  ];

  programs.tmux = {
    enable = true;
    clock24 = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "22.05"; # Did you read the comment?
}
