#!/usr/bin/env bash

# Installs NixOS on a SX134 Hetzner server, wiping the server.
#
# This is for a specific server configuration; adjust where needed.
#
# Usage:
#     ssh root@YOUR_SERVERS_IP bash -s < provision.sh
#
# * FIXME: encrypted zfs pools must be created manually so the user can input a
#   passphrase. the script will fail if this happens automatically because
#   there's no input.
#
# * A root user with empty password is created, so that you can just login
#   as root and press enter when using the Hetzner spider KVM.
#   Of course that empty-password login isn't exposed to the Internet.
#   Change the password afterwards to avoid anyone with physical access
#   being able to login without any authentication.

export LC_ALL=C

###: CONFIGURATION =======================================================

export MY_HOSTNAME=tapestone


##: --- Networking ---

export IPV4_ADDR="94.130.220.154"
export IPV4_CIDR="26"
export IPV4_GATEWAY="94.130.220.129"

export IPV6_ADDR="2a01:4f8:13b:17ac::1"
export IPV6_SUBNET="2a01:4f8:13b:17ac::/64"
export IPV6_CIDR="64"
export IPV6_GATEWAY="fe80::1"

# Cloudflare nameservers.
export NSV4="1.1.1.1" # also 1.0.0.1
export NSV6="2606:4700:4700::1111"


##: --- Devices ---

# boot/root
export NVME1="/dev/disk/by-id/nvme-SAMSUNG_MZQL2960HCJR-00A07_S64FNE0R701851"
export NVME2="/dev/disk/by-id/nvme-SAMSUNG_MZQL2960HCJR-00A07_S64FNE0R701889"

# silo
export HDD01="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A04SFVNG"
export HDD02="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A058FVNG"
export HDD03="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A04YFVNG"
export HDD04="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A04NFVNG"
export HDD05="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A04DFVNG"
export HDD06="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A053FVNG"
export HDD07="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A04TFVNG"
export HDD08="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1W0A00HFVNG"
export HDD09="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A02AFVNG"
export HDD10="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A05YFVNG"


##: --- ZFS Configuration ---
#
# Required for OpenZFS. Must be unique across all machines.
#
# Contrary to many explanations out there, according to OpenZFS, this does not
# need to be entirely numeric.

# Generates a random host ID for uniqueness across each run.
# FIXME: strip spaces! results in bad id
MY_HOSTID="$(tr -dc 0-9a-f < /dev/urandom | head -c 8)"

# For a consistent host ID, use this alternative.
# MY_HOSTID="$(head -c 8 /etc/machine-id)"

export MY_HOSTID

export ZFS_ENC_OPTS="-o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt"


##: --- Helper Functions ---

# Wrapper for parted >= 3.3 that does not exit 1 when it cannot inform
# the kernel of partitions changing (we use partprobe for that).
parted_nice() {
  parted "$@" 2> parted-stderr.txt || {
    grep "unable to inform the kernel of the change" parted-stderr.txt \
      || echo >&2 "Parted failed; stderr: $(< parted-stderr.txt)"
  }
}

# Create and mount a new ZFS pool.
zup() {
  local pool=$1
  local mountpoint=$2
  shift 2
  zfs create -p -o canmount=on -o mountpoint=legacy "$@" "$pool"
  mkdir -p "$mountpoint"
  mount -t zfs "$pool" "$mountpoint"
}


###: PREPARE ENVIRONMENT =======================================================

# Hetzner has some weird symlinks to make you install zfs with their script
rm /usr/local/sbin/zfs || true
rm /usr/local/sbin/zpool || true

cat > /etc/apt/preferences.d/90_zfs <<EOF
Package: libnvpair1linux libnvpair3linux libuutil1linux libuutil3linux libzfs2linux libzfs4linux libzpool2linux libzpool4linux spl-dkms zfs-dkms zfs-test zfsutils-linux zfsutils-linux-dev zfs-zed
Pin: release n=bullseye-backports
Pin-Priority: 990
EOF

apt update -y
apt install -y dpkg-dev "linux-headers-$(uname -r)" linux-image-amd64 sudo parted zfs-dkms zfsutils-linux


###: CONFIGURATION =======================================================

# source ./config.sh


###: FORMAT/PARTITION/MOUNT =======================================================

# source ./format-partition.zfs.sh


###: CREATE INITRD SSH KEY =====================================================

ssh-keygen -t ed25519 -N '' -f /mnt/boot/initrd-ssh-key
cp -v /mnt/boot/initrd-ssh-key* /mnt/boot-fallback/


###: INSTALL NIX ===============================================================

mkdir -p /etc/nix
# Let root run nix
echo "build-users-group =" > /etc/nix/nix.conf

curl -L https://nixos.org/nix/install | sh
set +u +x # sourcing this may refer to unset variables that we have no control over
. $HOME/.nix-profile/etc/profile.d/nix.sh
set -u -x

# FIXME: could this have a negative effect on initial installation root detection? probably not, but i'm superstitious.
# echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

nix-channel --add https://nixos.org/channels/nixos-22.05 nixpkgs
nix-channel --update

nix-env -iE "_: with import <nixpkgs/nixos> { configuration = {}; }; with config.system.build; [ nixos-generate-config nixos-install nixos-enter manual.manpages ]"


###: PREPARE NIXOS CONFIGURATION ===============================================

nixos-generate-config --root /mnt

# Generate `configuration.nix`. Note that we splice in shell variables.
cat > /mnt/etc/nixos/configuration.nix <<EOF
{ config, pkgs, ... }:
let
  networkInterface = "enp7s0";
  ipv4 = {
    address = "$IPV4_ADDR";
    gateway = "$IPV4_GATEWAY";
    prefixLength = $IPV4_CIDR;
  };
  ipv6 = {
    address = "$IPV6_ADDR";
    gateway = "$IPV6_GATEWAY";
    prefixLength = $IPV6_CIDR;
  };
  authorizedKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAPdEosvv8H1UpHC725ZTBRY0L6ufn8MU2UEmI1JN1VL xtallos@parrothelles"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDwOUQFOaTPMtYG4VWrgHF772sf4MhmK5Rvq4vlUFFXH hierophant@loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP5ffhsQSZ3DsVddNzfsahN84SFnDWn9erSXiKbVioWy hierophant.loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH2CtLx2fSUVaU1gJXqXHpGbfhkj0XV8NotIuXF76DWj seadoom@boschic.loop.garden"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG+iDtB1+DXl89xmlHz6irAYfI2dm4ubinsH3apMeFeo seadoom@hodgepodge.loop.garden"
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
        devices = ["$NVME2"];
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
  };

  networking.hostName = "$MY_HOSTNAME";
  networking.hostId = "$MY_HOSTID";

  # Hetzner uses static IP assignments.
  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = false;
  networking.interfaces."eth0".ipv4.addresses = [{inherit (ipv4) address prefixLength;}];
  networking.interfaces."eth0".ipv6.addresses = [{inherit (ipv6) address prefixLength;}];
  networking.defaultGateway = ipv4.gateway;
  networking.defaultGateway6 = {
    address = ipv6.gateway;
    interface = "eth0";
  };
  networking.nameservers = [ "1.1.1.1" "1.0.0.1" ];

  users.users.root.initialHashedPassword = "";
  services.openssh.permitRootLogin = "prohibit-password";
  users.users.root.openssh.authorizedKeys.keys = authorizedKeys;

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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "22.05"; # Did you read the comment?
}
EOF

# Pre-flight check to prevent issues with missing files during install.
# https://discourse.nixos.org/t/nixos-21-05-installation-failed-installing-from-an-existing-distro/13627/3
# https://github.com/NixOS/nixpkgs/issues/126141#issuecomment-861720372
nix-build '<nixpkgs/nixos>' -A config.system.build.toplevel -I nixos-config=/mnt/etc/nixos/configuration.nix

# Installation fails without this.
mkdir -p /mnt/tmp

# Install NixOS
PATH="$PATH" NIX_PATH="$NIX_PATH" "$(command -v nixos-install)" \
  --no-root-passwd \
  --root /mnt \
  --max-jobs "$(nproc)"

# if you need to debug something
# - connect to the rescue system
# - install zfs
# ```
# zpool import -f rpool temp_rpool
# mount -t zfs temp_rpool/local/root /mnt
# journalctl --directory=/mnt/var/log/journal
