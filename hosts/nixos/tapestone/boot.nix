# FIXME: CANNOT BOOT WITHOUT KVM CONSOLE!
# no shell access to zfs decryption prompt in initrd...
{
  config,
  lib,
  pkgs,
  ...
}: let
  port = 22;
  authorizedKeys = import ../../../identity/authorized-keys.nix;
  torRc = pkgs.writeText "tor.rc" ''
    DataDirectory /etc/tor
    SOCKSPort 127.0.0.1:9050 IsolateDestAddr
    SOCKSPort 127.0.0.1:9063
    HiddenServiceDir /etc/tor/onion/bootup
    HiddenServicePort ${port} 127.0.0.1:${port}
  '';
in {
  boot.initrd = {
    availableKernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "usbhid"
      "uas"
      "sd_mod"
      # Ensure the network adapter is usable during stage 1.
      # $ lspci -v
      "igb"
    ];
    kernelModules = [];

    extraUtilsCommands = ''
      copy_bin_and_libs ${pkgs.tor}/bin/tor

      # Gather entropy for faster tor startup
      copy_bin_and_libs ${pkgs.haveged}/bin/haveged
    '';

    # Configure stage-1 networking so we can decrypt the drives prior to boot.
    network = {
      enable = true;
      ssh = {
        inherit
          authorizedKeys
          port
          ;
        enable = true;
        hostKeys = [
          /etc/secrets/initrd/ssh_host_rsa_key
          /etc/secrets/initrd/ssh_host_ed25519_key
        ];
      };
      postCommands = ''
        echo "[tor]: preparing onion folder..."
        chmod -R 700 /etc/tor

        echo "[net]: force localhost up..."
        ip a a 127.0.0.1/8 dev lo
        ip link set lo up

        echo "[haveged]: gathering entropy..."
        haveged -F &

        echo "[tor]: starting tor..."
        tor -f ${torRc} --verify-config
        tor -f ${torRc} &

        zpool import spool
        echo "zfs load-key -a; killall zfs" >> /root/.profile
      '';
    };

    secrets = {
      "/etc/tor/onion/bootup" = /persist/tor/onion;
    };
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    device = "nodev";
    mirroredBoots = [
      {
        devices = [config.fileSystems."/boot-fallback".device];
        path = "/boot-fallback";
      }
    ];
  };

  boot.extraModulePackages = [];
}
