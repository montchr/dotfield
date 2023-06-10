#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

# Provisions NixOS on a Hetzner Cloud instance, wiping the server.

# Hardware data:
#
#    CPU1: AMD EPYC-Milan Processor (Cores 2)
#    Memory:  7770 MB
#    Disk /dev/sda: 81 GB (=> 76 GiB)
#    Disk /dev/sdb: 85 GB (=> 80 GiB) doesn't contain a valid partition table
#    Disk /dev/sdc: 17 GB (=> 16 GiB)
#    Disk /dev/sdd: 17 GB (=> 16 GiB)
#    Total capacity 188 GiB with 4 Disks
#
# Network data:
#    eth0  LINK: yes
#          MAC:  96:00:02:3b:51:e7
#          IP:   37.27.17.58
#          IPv6: 2a01:4f9:c010:ecae::2/64
#          Virtio network driver

export LC_ALL=C

apt update -y
apt install -y dpkg-dev "linux-headers-$(uname -r)" linux-image-amd64 sudo parted


###: CONFIGURATION =======================================================

export NEW_HOSTNAME=moraine-dev

##: --- Devices ---

export FSOPTS="defaults,x-mount.mkdir,noatime"
export BTRFSOPTS="${FSOPTS},compress=zstd"

export LOCAL_PREFIX="/mnt/local"

# /dev/sda
export DISK_ROOT_1="/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_32559184"

# /dev/sdd :: scsi-SHC_Volume_32566072
export VOL_ROOT_2="/dev/disk/by-id/scsi-0HC_Volume_32566072"

# /dev/sdb :: scsi-SHC_Volume_32559056
export VOL_LOCAL_1="/dev/disk/by-id/scsi-0HC_Volume_32559056"

# /dev/sdc :: scsi-SHC_Volume_32559057
export VOL_LOCAL_2="/dev/disk/by-id/scsi-0HC_Volume_32559057"

export PRIMARY_DEVICES=(
  "$DISK_ROOT_1"
  "$VOL_ROOT_2"
)

export SECONDARY_DEVICES=(
  "$VOL_LOCAL_1"
  "$VOL_LOCAL_2"
)

export BIOS_PART=1
export EFI_PART=2
export ROOT_PART=3

##: --- Helper Functions ---

# Wrapper for parted >= 3.3 that does not exit 1 when it cannot inform
# the kernel of partitions changing (we use partprobe for that).
parted_nice() {
  parted "$@" 2>parted-stderr.txt || {
    grep "unable to inform the kernel of the change" parted-stderr.txt \
      || echo >&2 "Parted failed; stderr: $(<parted-stderr.txt)"
  }
}

###: FORMAT/PARTITION/MOUNT =======================================================

# Inspect existing disks
lsblk
ls -l /dev/disk/by-id

vgchange -an || true

# Stop all mdadm arrays that the boot may have activated.
mdadm --stop --scan

# Prevent mdadm from auto-assembling arrays.
#
# Otherwise, as soon as we create the partition tables below, it will try to
# re-assemple a previous RAID if any remaining RAID signatures are present,
# before we even get the chance to wipe them.
#
# Source: <https://unix.stackexchange.com/questions/166688/prevent-debian-from-auto-assembling-raid-at-boot/504035#504035>
#
# We use `>` because the file may already contain some detected RAID arrays,
# which would take precedence over our `<ignore>`.
echo 'AUTO -all
ARRAY <ignore> UUID=00000000:00000000:00000000:00000000' >/etc/mdadm/mdadm.conf


##: --- VOLUME: 'NIXOS' ---------------------------------------------------------

for primary in "${PRIMARY_DEVICES[@]}"; do
  # N.B. untested/aspirational!
  sgdisk --zap-all "$primary"
  parted_nice --script "$primary" mklabel gpt
  # Wipe any previous RAID/ZFS signatures
  wipefs --all --force "$primary"

  sgdisk -n1:0:+4M -t1:EF02 "$primary"  # bios
  sgdisk -n2:0:+2G -t2:EF00 "$primary"  # efi/esp
  sgdisk -n3:0:0 -t3:8300 "$primary"    # root

  partprobe

  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists="${primary}-part1"

  mkfs.fat -F 32 -n EFI "${primary}-part${EFI_PART}"
done

# NOTE: `--metadata` was originally `single` but the default `raid1` would be preferable,
#       as long as it doesn't interfere with boot.
mkfs.btrfs --force \
  --label nixos \
  --data single \
  "${DISK_ROOT_1}-part${ROOT_PART}" \
  "${VOL_ROOT_2}-part${ROOT_PART}"

btrfs device scan

mkdir -p /mnt
mount -t btrfs LABEL=nixos /mnt

btrfs subvolume create /mnt/@root
btrfs subvolume create /mnt/@store
btrfs subvolume create /mnt/@log
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@persist
btrfs subvolume create /mnt/@mysql
btrfs subvolume create /mnt/@postgres

# Create a read-only snapshot of the `@root` subvolume for impermanace.
btrfs subvolume snapshot -r /mnt/@root /@root-blank

btrfs subvolume list -a /mnt
umount /mnt

mount -t btrfs -o "subvol=@root,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt

mount -t btrfs -o "subvol=@store,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt/nix

mount -t btrfs -o "subvol=@log,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt/var/log

mount -t btrfs -o "subvol=@home,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt/home

mount -t btrfs -o "subvol=@persist,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt/persist

mount -t btrfs -o "subvol=@mysql,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt/var/lib/mysql

mount -t btrfs -o "subvol=@postgres,ssd,${FSOPTS}" LABEL="nixos" \
  /mnt/var/lib/postgres

# Creating file systems changes their UUIDs.
# Trigger udev so that the entries in /dev/disk/by-uuid get refreshed.
# `nixos-generate-config` depends on those being up-to-date.
# See https://github.com/NixOS/nixpkgs/issues/62444
udevadm trigger

mkdir -p /mnt/boot
mount "${DISK_ROOT_1}-part${BIOS_PART}" /mnt/boot


##: --- VOLUME: 'LOCAL' ----------------------------------------------------------

mkfs.btrfs --force \
  --label local \
  --data raid1 \
  --metadata raid1 \
  "${VOL_LOCAL_1}-part1" \
  "${VOL_LOCAL_2}-part1"

udevadm trigger
btrfs device scan

mkdir -p "${LOCAL_PREFIX}"
mount -t btrfs LABEL=local "${LOCAL_PREFIX}"
btrfs subvolume create "${LOCAL_PREFIX}/@backups"
btrfs subvolume create "${LOCAL_PREFIX}/@downloads-completed"
btrfs subvolume create "${LOCAL_PREFIX}/@music"
btrfs subvolume create "${LOCAL_PREFIX}/@movies"
btrfs subvolume create "${LOCAL_PREFIX}/@tv-shows"
btrfs subvolume list -a "${LOCAL_PREFIX}"
umount "${LOCAL_PREFIX}"

mount -t btrfs -o "subvol=@backups,${FSOPTS}" LABEL="local" \
  "${LOCAL_PREFIX}/backups"
mount -t btrfs -o "subvol=@downloads-completed,${FSOPTS}" LABEL="local" \
  "${LOCAL_PREFIX}/downloads/completed"
mount -t btrfs -o "subvol=@music,${FSOPTS}" LABEL="local" \
  "${LOCAL_PREFIX}/media/music"
mount -t btrfs -o "subvol=@movies,${FSOPTS}" LABEL="local" \
  "${LOCAL_PREFIX}/media/movies"
mount -t btrfs -o "subvol=@tv-shows,${FSOPTS}" LABEL="local" \
  "${LOCAL_PREFIX}/media/tv-shows"


###: INSTALL NIX ===============================================================

mkdir -p /etc/nix
# Let root run nix
echo "build-users-group =" >/etc/nix/nix.conf

curl -L https://nixos.org/nix/install | sh

# Make Nix available for immediate usage.
#
# As this is a third-party script, sourcing may refer to unset variables, so
# loosen up "strict mode".
#
# TODO: is it really necessary to set `+x`?
set +u +x
# shellcheck disable=SC1091
. "$HOME/.nix-profile/etc/profile.d/nix.sh"
set -u -x

echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

# NOTE: 23.05 is scheduled for release <2023-05-31>
nix-channel --add https://nixos.org/channels/nixos-23.05 nixpkgs
nix-channel --update

# Open a Nix shell with the NixOS installation dependencies.
# TODO: use `nix profile install`?
nix-env -iE "_: with import <nixpkgs/nixos> { configuration = {}; }; with config.system.build; [ nixos-generate-config nixos-install nixos-enter manual.manpages git ]"


###: PREPARE NIXOS CONFIGURATION ===============================================

git clone https://git.sr.ht/~montchr/dotfield /mnt/etc/dotfield
ln -s /mnt/etc/dotfield /mnt/etc/nixos

nixos-generate-config --root /mnt

nix build "/mnt/etc/nixos#nixosConfigurations.${NEW_HOSTNAME}.config.system.build.toplevel"

PATH="$PATH" "$(command -v nixos-install)" \
  --no-root-passwd \
  --root /mnt \
  --max-jobs "$(nproc)"
