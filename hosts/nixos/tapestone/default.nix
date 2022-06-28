# FIXME: CANNOT BOOT WITHOUT KVM CONSOLE!
# no shell access to zfs decryption prompt in initrd...

{ config, pkgs, peers, ... }:
let
  inherit (peers.hosts.tapestone) ipv4 ipv6;
  interface = "eth0";
  authorizedKeys = import ../../../identity/authorized-keys.nix;
in
{
  imports = [
    ./boot.nix
    ./filesystems.nix
    ./hardware-configuration.nix
    ./zfs.nix
  ];

  networking.hostName = "tapestone";
  # networking.domain = "cube.garden";
  networking.hostId = "93e48b92";

  # Hetzner uses static IP assignments.
  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = false;

  networking.defaultGateway = { inherit interface; address = ipv4.gateway; };
  networking.defaultGateway6 = {inherit interface; address = ipv6.gateway;};

  networking.interfaces.${interface} = {
    ipv4.addresses = [{inherit (ipv4) address prefixLength;}];
    ipv6.addresses = [{
      inherit (ipv6) prefixLength;
      address = "${ipv6.subnet}::1";
    }];
  };

  # FIXME: re-enable?
  # networking.firewall.allowedTCPPorts = [ 22 2222 ];

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

  environment.systemPackages = with pkgs; [
    vim
    tealdeer
    fish
    git
    tor
    screen
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
