{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: let
  inherit (builtins) toString;
  inherit (primaryUser) authorizedKeys;
  port = 22;
  torRc = pkgs.writeText "tor.rc" ''
    DataDirectory /etc/tor
    SOCKSPort 127.0.0.1:9050 IsolateDestAddr
    SOCKSPort 127.0.0.1:9063
    HiddenServiceDir /etc/tor/onion/bootup
    HiddenServicePort ${toString port} 127.0.0.1:${toString port}
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

    luks.cryptoModules = ["aes" "sha512" "sha1" "xts"];

    extraUtilsCommands = ''
      copy_bin_and_libs ${pkgs.tor}/bin/tor

      # Gather entropy for faster tor startup
      copy_bin_and_libs ${pkgs.haveged}/bin/haveged
    '';

    network = {
      enable = true;
      ssh = {
        inherit authorizedKeys port;
        enable = true;
        hostKeys = [
          "/persist/etc/secrets/initrd/ssh_host_rsa_key"
          "/persist/etc/secrets/initrd/ssh_host_ed25519_key"
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
      '';
    };

    secrets = {
      "/etc/tor/onion/bootup" = /persist/etc/tor/onion/bootup;
    };
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.grub = {
    enable = false;
    version = 2;
    efiSupport = true;
    device = "nodev";
    copyKernels = true;
    # mirroredBoots = [
    #   {
    #     devices = [config.fileSystems."/boot-fallback".device];
    #     path = "/boot-fallback";
    #   }
    # ];
  };

  boot.extraModulePackages = [];
}
