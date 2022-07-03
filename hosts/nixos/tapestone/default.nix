{
  config,
  pkgs,
  peers,
  lib,
  profiles,
  hmUsers,
  ...
}: let
  inherit (peers.hosts.tapestone) ipv4 ipv6;
  interface = "eth0";
  authorizedKeys = import ../../../identity/authorized-keys.nix;
in {
  imports =
    (with profiles; [hardware.amd])
    ++ [
      ./boot.nix
      ./filesystems.nix
      ./hardware-configuration.nix
      ./network.nix
    ];

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = authorizedKeys;
  users.users.root.initialHashedPassword = "$6$.CAfJuUGGl2hZKVT$Dz4JuY4RFcuEIgDaXy9Ru7b9XPwdsA1NmgNY3CQ8R89ozatksh0TH6x3DkGJ9lz7jfP/Y6grvjqSDQuqmxwwH.";
  users.users.seadoom = {
    uid = 1000;
    extraGroups = ["wheel"];
    initialHashedPassword = "$6$vKTiMLXS2fS8EfAf$cttVu8Gvy5E0sM.qlU.VwrZcnPyjN/DNVY0Mjv5ePMcy.NSrXaWqYU6LvqSIsHmlDgDjrxChUafd5Wn0Y2Unh0";
    isNormalUser = true;
    openssh.authorizedKeys.keys = authorizedKeys;
  };
  home-manager.users.seadoom = hmArgs: {
    imports =
      (with hmArgs.suites; server)
      ++ [hmUsers.seadoom];
  };

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
