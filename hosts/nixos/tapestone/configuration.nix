# FIXME: CANNOT BOOT WITHOUT KVM CONSOLE!
# no shell access to zfs decryption prompt in initrd...

{ config, pkgs, ... }:
let
  # interface = "enp7s0";
  interface = "eth0";
  ipv4 = {
    address = "94.130.220.154";
    gateway = "94.130.220.129";
    prefixLength = 26;
  };
  ipv6 = {
    address = "2a01:4f8:13b:17ac::1";
    gateway = "fe80::1";
    prefixLength = 64;
  };
  authorizedKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAPdEosvv8H1UpHC725ZTBRY0L6ufn8MU2UEmI1JN1VL xtallos@parrothelles"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDwOUQFOaTPMtYG4VWrgHF772sf4MhmK5Rvq4vlUFFXH hierophant@loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP5ffhsQSZ3DsVddNzfsahN84SFnDWn9erSXiKbVioWy hierophant.loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH2CtLx2fSUVaU1gJXqXHpGbfhkj0XV8NotIuXF76DWj seadoom@boschic.loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG+iDtB1+DXl89xmlHz6irAYfI2dm4ubinsH3apMeFeo seadoom@HodgePodge.loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG2HrKDL60obU2mEkV1pM1xHQeTHc+czioQDTqu0gP37 blink@aerattum"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCrU4ZPmxcBNnMeLLyBkFcjlG2MwaIUp5deSycmXSb7gIC4MZKH0lvoCXsXBYTocGhwna2mg1SfpolLZzxzWAYpx52RoHeyY6ml/Z1dSJbpMgV5KZ2kqKo1hHar2i9wsc/EZQKv3rlngOSECiwg2LxHOIGGTz/779yEJnfnWnta+5Tnpk4zdgp8j8g+QbY7NFHcZg2mjcy++Nf2psqJsDZVE1JmzNsA30jEGaGDRAaAv9ZHcQf6E3GEpRvr3iqO9YTzOcgdzzl8CvAtZUa1G4piQK6CYkC6HgAvm73+kSm+JxssSfFi3xgK0+RLAUTGa25MH3PAqR9V8lrcuLI891sLEQTtQIIALfzTw04e740DqXRifzasCVo8lMmZBX8Mu+FC0KSFL0254OfHuTHDCWE7fc/3069pcpgAaJGIDj2rE3v631WqoPZpkmvefuu4+n5nvKe4ypwA/OH6h52s3CL7DlcREe6lnBraEzbuXxVL+0JP66yEzK4vFGtZWeTsbo9jyQkoJIw4IkuqHvRxElysOHaQqG08GkjiCBONiGIqk0GQ3pmeyjptfnrVyi2pFGTvVVQ06ZC7If3wywkWXCJzJ2nrD9B+gyRvKv557m24Goj2+LCi6IVZsFIh6r4+vOdaMnX39eol/kWMl1n93D8YG3bBS5JH0fEQsMZEpsUd7Q== WorkingCopy@aerattum"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3tWcOwvNOfHXX3YvtLmJRigxATUh++bWRCAM07uy3mbNvEteT5bF/7nixO44gep0Hv24jaqLeGjCaTxFXrmt1NGgvmAXcsoS4I3+N2xfiFZPIKoiF0EONDsInjm4h5eNoPPE4Rd9xLju4S4tXaXDcL37PunQZJ+aR6CRVf/geM+H4y70cvYHV6uakMAfuv/0+AEMLwlSIN7OpDN8B+JGI4rQhBsekRkkkcZlPYO4vT63aTvLCYFxJ/fR45oMKW57lvZUrbRMHbKRkOfyhBF3qbYR/9aMEUd7gjYBfLJ1hQaHlp2aV49m53WFBjmjqjFcxDPxS/HMk/Hazowkw0G6iNzSNHnO5wI/BxIEahavYvd4VOQXpaWs/G58t8kdQol8WFufLjAReP0j16TqcWEHwy1ktMcrpYfDlLSlNcuaUeXJNIyvD3WmfRDXBnxlBenFIqe9lnK8RUVCcxM+lEEJbMWs1ZuWmgXjbt3UkFhSKSv2Adlm2/OfBBCyO46hVmhLfkwzB69aXYqUjPthlvtCDuLxrmT+DZeWsucUKPp2L9PXS6LpbpnIWCqmnGIPLjHBX2X3EOKwrtLAGN5wv7zLv88qHOD0MET2KVZkfTLg04FkcNowNwAlQ8xBBjpt6xEWNFMH532ZRO1CT0VTUNB7nEW2JET1SULsRT/bTUbKQHQ== yk5cNfc"
  ];
in
{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    device = "nodev";
    mirroredBoots = [
      {
        devices = ["/dev/disk/by-id/nvme-SAMSUNG_MZQL2960HCJR-00A07_S64FNE0R701889"];
        path = "/boot-fallback";
      }
    ];
  };
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.enableUnstable = true;
  boot.zfs.requestEncryptionCredentials = true;

  # Continue booting regardless of the availability of the mirrored boot
  # partitions. We don't need both.
  fileSystems."/boot".options = ["nofail"];
  fileSystems."/boot-fallback".options = ["nofail"];

  # Configure stage-1 networking so we can decrypt the drives prior to boot.
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
  boot.initrd.availableKernelModules = [ "igb" ];
  # boot.initrd.kernelModules = ["e1000e"];

  # TODO: configure mail sending
  nixpkgs.config.packageOverrides = pkgs: {
    zfsStable = pkgs.zfsStable.override { enableMail = true; };
    zfsUnstable = pkgs.zfsUnstable.override { enableMail = true; };
  };

  networking.hostName = "tapestone";
  networking.hostId = "93e48b92";

  # Hetzner uses static IP assignments.
  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = false;
  networking.interfaces.${interface} = {
    ipv4.addresses = [{inherit (ipv4) address prefixLength;}];
    ipv6.addresses = [{inherit (ipv6) address prefixLength;}];
  };
  networking.defaultGateway = ipv4.gateway;
  networking.defaultGateway6 = {
    inherit interface;
    address = ipv6.gateway;
  };
  networking.nameservers = [ "1.1.1.1" "8.8.8.8" ];

  services.openssh.permitRootLogin = "prohibit-password";
  services.openssh.openFirewall = true;
  # networking.firewall.allowedTCPPorts = [ 22 2222 ];
  # users.users.root.openssh.authorizedKeys.keys = authorizedKeys;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

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

  services.openssh.enable = true;

  # ZFS maintenance settings.
  services.zfs.trim.enable = true;
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoScrub.pools = [ "rpool" "spool" ];

  # ZFS already has its own scheduler.
  # https://nixos.wiki/wiki/ZFS#How_to_use_it
  services.udev.extraRules = ''
    ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
  '';

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
