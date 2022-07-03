#!/usr/bin/env bash

set -euox pipefail

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

export -f parted_nice
export -f zup

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
  parted_nice --script --align optimal $nvme -- \
    mklabel gpt \
    mkpart NIXOS_BOOT 1MB 1025MB set 1 boot on \
    mkpart nixos 1026MB '100%'
done

for disk in $HDD01 $HDD02 $HDD03 $HDD04 $HDD05 $HDD06 $HDD07 $HDD08 $HDD09 $HDD10; do
  parted_nice --script --align optimal $disk -- \
    mklabel gpt \
    mkpart 'silo' 1MB '100%'
done

partprobe

for nvme in $NVME1 $NVME2; do
  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists=$nvme-part1
  udevadm settle --timeout=5 --exit-if-exists=$nvme-part2

  # Wipe any previous RAID signatures
  mdadm --zero-superblock --force $nvme-part1 || true

  # Filesystems (-F to not ask on preexisting FS)
  mkfs.fat -F 32 -n EFI $nvme-part1
done

for disk in $HDD01 $HDD02 $HDD03 $HDD04 $HDD05 $HDD06 $HDD07 $HDD08 $HDD09 $HDD10; do
  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists=$disk-part1
done

# Creating file systems changes their UUIDs.
# Trigger udev so that the entries in /dev/disk/by-uuid get refreshed.
# `nixos-generate-config` depends on those being up-to-date.
# See https://github.com/NixOS/nixpkgs/issues/62444
udevadm trigger

# FIXME: for some reason, these aren't available initially, and these commands need to be run a second time...
mkdir -p /mnt/boot{-fallback,}
mount $NVME1-part1 /mnt/boot
mount $NVME2-part1 /mnt/boot-fallback


###: INITIALIZE 'ROOT' POOL ====================================================

zpool create \
  -o ashift=12 \
  -o autotrim=on \
  -O acltype=posixacl \
  -O atime=off \
  -O canmount=off \
  -O dnodesize=auto \
  -O mountpoint=none \
  -O normalization=formD \
  -O relatime=on \
  -O xattr=sa \
  -f \
  rpool \
  mirror \
  $NVME1-part2 $NVME2-part2

# Reserve 1GB of space for ZFS operations -- even delete requires free space
zfs create \
  -o refreservation=1G \
  -o mountpoint=none \
  rpool/reserved

zfs create \
  -o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt \
  -o mountpoint=none \
  rpool/local

zfs create \
  -o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt \
  -o mountpoint=none \
  -o "com.sun:auto-snapshot=true" \
  rpool/safe

zup rpool/local/root /mnt

# Take an initial snapshot with the root dataset before mounting anything else.
zfs snapshot rpool/local/root@blank

zup rpool/local/nix /mnt/nix

zup rpool/safe/home /mnt/home
zup rpool/safe/persist /mnt/persist

# Create a special volume optimized for databases
# https://wiki.archlinux.org/index.php/ZFS#Databases
zup rpool/safe/postgres /mnt/var/lib/postgres \
  -o recordsize=8K \
  -o primarycache=metadata \
  -o logbias=throughput


###: INITIALIZE 'SILO' POOL ====================================================

zpool create \
  -o ashift=12 \
  -o autotrim=on \
  -O acltype=posixacl \
  -O atime=off \
  -O canmount=off \
  -O dnodesize=auto \
  -O mountpoint=none \
  -O normalization=formD \
  -O relatime=on \
  -O xattr=sa \
  -f \
  spool raidz \
  $HDD01-part1 $HDD02-part1 $HDD03-part1 $HDD04-part1 $HDD05-part1 $HDD06-part1 $HDD07-part1 $HDD08-part1 $HDD09-part1 $HDD10-part1

zup spool/backup /mnt/silo/backup \
  -o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt

zup spool/data /mnt/silo/data \
  -o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt \
  -o "com.sun:auto-snapshot=true"
