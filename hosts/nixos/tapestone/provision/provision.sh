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

# Mount an existing ZFS pool.
zmnt() {
  local pool=$1
  local mountpoint=$2
  mkdir -p "$mountpoint"
  mount -t zfs "$pool" "$mountpoint"
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

btrmnt() {
  local subvol=$1
  local mountpoint=$2
  shift 2

  local extraOptions=$*
  local point="/mnt${mountpoint}"

  mkdir -p "${point}"
  mount -o "compress=zstd,subvol=${subvol},${extraOptions}" \
    /dev/disk/by-label/nixos \
    "${point}"
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


###: FORMAT/PARTITION/MOUNT =======================================================

# Inspect existing disks
lsblk
ls /dev/disk/by-id

# Undo existing setups to allow running the script multiple times to iterate on it.
# We allow these operations to fail for the case the script runs the first time.
umount /mnt || true
umount /mnt/boot || true
umount /mnt/boot/efi || true
umount /mnt/silo || true
umount /mnt/nix || true
umount /mnt/home || true
umount /mnt/persist || true

vgchange -an || true

# Stop all mdadm arrays that the boot may have activated.
mdadm --stop --scan

# Prevent mdadm from auto-assembling arrays.
# Otherwise, as soon as we create the partition tables below, it will try to
# re-assemple a previous RAID if any remaining RAID signatures are present,
# before we even get the chance to wipe them.
# From:
#     https://unix.stackexchange.com/questions/166688/prevent-debian-from-auto-assembling-raid-at-boot/504035#504035
# We use `>` because the file may already contain some detected RAID arrays,
# which would take precedence over our `<ignore>`.
echo 'AUTO -all
ARRAY <ignore> UUID=00000000:00000000:00000000:00000000' > /etc/mdadm/mdadm.conf


# Create partition tables (--script to not ask)
parted_nice --script $NVME1 mklabel gpt
parted_nice --script $NVME2 mklabel gpt

parted_nice --script $HDD01 mklabel gpt
parted_nice --script $HDD02 mklabel gpt
parted_nice --script $HDD03 mklabel gpt
parted_nice --script $HDD04 mklabel gpt
parted_nice --script $HDD05 mklabel gpt
parted_nice --script $HDD06 mklabel gpt
parted_nice --script $HDD07 mklabel gpt
parted_nice --script $HDD08 mklabel gpt
parted_nice --script $HDD09 mklabel gpt
parted_nice --script $HDD10 mklabel gpt


# Create partitions with GNU `parted`.
#
# - Leaves space for the 1MB BIOS boot partition at the front, "just in case".
#
# ## Notes
#
# - Use "MB" instead of "MiB" because otherwise `--align optimal` has no effect.
#
#   From <https://www.gnu.org/software/parted/manual/html_node/unit.html#unit>:
#
#   > Note that as of parted-2.4, when you specify start and/or end values using IEC
#   > binary units like "MiB", "GiB", "TiB", etc., parted treats those values as exact
#
# - When using `mkpart` on GPT, the first argument to `mkpart` is not a
#   `part-type`, but the GPT partition name.
#
#   From <https://www.gnu.org/software/parted/manual/html_node/mkpart.html#mkpart>:
#
#   > ... part-type is one of 'primary', 'extended' or 'logical', and may be specified only with 'msdos' or 'dvh' partition tables.
#   > A name must be specified for a 'gpt' partition table.
#
# - GPT partition names are limited to 36 UTF-16 chars.
#   See <https://en.wikipedia.org/wiki/GUID_Partition_Table#Partition_entries_(LBA_2-33)>.
#

for nvme in $NVME1 $NVME2; do
  # N.B. untested/aspirational!
  sgdisk --zap-all $nvme
  sgdisk -n1:0:+4M  -t1:EF02 $nvme  # bios
  sgdisk -n2:0:+2G  -t2:EF00 $nvme  # esp
  sgdisk -n3:0:+11G -t3:8200 $nvme  # swap
  sgdisk -n4:0:0    -t4:8300 $nvme  # root
done

for disk in $HDD01 $HDD02 $HDD03 $HDD04 $HDD05 $HDD06 $HDD07 $HDD08 $HDD09 $HDD10; do
  sgdisk --zap-all $disk
  sgdisk -n1:0:0    -t1:8300 $disk
done

partprobe

for nvme in $NVME1 $NVME2; do
  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists=$nvme-part1
  udevadm settle --timeout=5 --exit-if-exists=$nvme-part2

  # Wipe any previous RAID signatures
  mdadm --zero-superblock --force $nvme-part1 || true

  # Filesystems (-F to not ask on preexisting FS)
  mkfs.fat -F 32 -n EFI $nvme-part2
done

for i in {01..10}; do
  diskVar="HDD${i}"
  disk="${!diskVar}"

  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists=$disk-part1

  cryptsetup luksFormat -h sha512 -s 512 -c aes-xts-plain64 $disk-part1
  cryptsetup open --type luks $disk-part1 "enc${i}"
done

# Creating file systems changes their UUIDs.
# Trigger udev so that the entries in /dev/disk/by-uuid get refreshed.
# `nixos-generate-config` depends on those being up-to-date.
# See https://github.com/NixOS/nixpkgs/issues/62444
udevadm trigger

mkdir -p /mnt/boot{-fallback,}
mount "${NVME1}-part2" /mnt/boot
mount "${NVME2}-part2" /mnt/boot-fallback

mkfs.btrfs -f -L nixos -d single \
  "${NVME1}-part4" \
  "${NVME2}-part4"

btrfs device scan

mkdir -p /mnt/btrfs

mount -t btrfs /dev/disk/by-label/nixos /mnt/btrfs

btrfs subvolume create /mnt/btrfs/local
btrfs subvolume create /mnt/btrfs/local/root
btrfs subvolume create /mnt/btrfs/local/nix
btrfs subvolume create /mnt/btrfs/local/log

btrfs subvolume create /mnt/btrfs/safe
btrfs subvolume create /mnt/btrfs/safe/home
btrfs subvolume create /mnt/btrfs/safe/persist
btrfs subvolume create /mnt/btrfs/safe/postgres

btrfs subvolume list -a /mnt/btrfs
umount /mnt

# Mount directly at /mnt
btrmnt local/root /
btrmnt local/nix /nix noatime
btrmnt local/log /var/log
btrmnt safe/home /home
btrmnt safe/persist /persist
# FIXME: add proper options
btrmnt safe/postgres /var/lib/postgres

##: 'SILO'

mkfs.btrfs -d raid10 -m raid10 -L silo /dev/mapper/enc*

mkdir -p /mnt/silo/btrfs
mount -t btrfs /dev/disk/by-label/silo /mnt/silo/btrfs
btrfs subvolume create /mnt/silo/btrfs/backup
btrfs subvolume create /mnt/silo/btrfs/data
btrfs subvolume create /mnt/silo/btrfs/data/media
umount /mnt/silo/btrfs

mount -o compress=zstd,subvol=backup \
  /dev/disk/by-label/silo \
  /mnt/silo/backup
mount -o compress=zstd,subvol=data/media \
  /dev/disk/by-label/silo \
  /mnt/silo/data/media


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

echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

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
PATH="$PATH" "$(command -v nixos-install)" \
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
