{ config, lib, pkgs, ... }:
let
  authorizedKeys = import ../../../identity/authorized-keys.nix;
in
{
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    # FIXME: is this really the right choice?
    device = "nodev";
    mirroredBoots = [
      {
        devices = ["/dev/disk/by-id/nvme-SAMSUNG_MZQL2960HCJR-00A07_S64FNE0R701889"];
        path = "/boot-fallback";
      }
    ];
  };

  # Configure stage-1 networking so we can decrypt the drives prior to boot.
  # FIXME: this only works with KVM access!
  boot.initrd.network.enable = true;
  boot.initrd.network.ssh = {
    inherit authorizedKeys;
    enable = true;
    port = 2222;
    hostKeys = [
      /etc/secrets/initrd/ssh_host_rsa_key
      /etc/secrets/initrd/ssh_host_ed25519_key
      /boot/initrd-ssh-key
      /boot-fallback/initrd-ssh-key
    ];
  };
  boot.initrd.network.postCommands = ''
    echo "zpool import spool && zfs load-key -a && killall zfs" >> /root/.profile
  '';

  # Ensure the network adapter is usable during stage 1.
  # FIXME: try enabling e1000e to fix network initrd?
  boot.initrd.availableKernelModules = [ "igb" ];
  # boot.initrd.kernelModules = ["e1000e"];

}
