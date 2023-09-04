#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

# Provisions NixOS on a customised Hetzner AX52 server, wiping the server.
#
# CPU:      AMD Ryzen™ 7 7700
# RAM:      64 GB DDR5 (+ECC)
# Storage:  2 x 1TB NVMe SSD (+ 2 x 16TB HDD)

export LC_ALL=C

apt update -y
apt install -y dpkg-dev "linux-headers-$(uname -r)" linux-image-amd64 sudo parted
apt install -y kitty-terminfo

###: CONFIGURATION =======================================================

export NEW_HOSTNAME=moraine

##: --- Devices ---

export FSOPTS="defaults,x-mount.mkdir,noatime"
export BTRFSOPTS="${FSOPTS},compress=zstd"

export LOCAL_PREFIX="/mnt/srv/"

# boot/root
export NVME01="/dev/nvme0n1"
export NVME02="/dev/nvme1n1"

# local data
export HDD01="/dev/sda"
export HDD02="/dev/sdb"

export NVMEXX=(
  "$NVME01"
  "$NVME02"
)

export HDDXX=(
  "$HDD01"
  "$HDD02"
)

export BIOS_PART=1
export EFI_PART=2
export ROOT_PART=3

export BOOT_DEV="${NVME01}p${EFI_PART}"

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
ls /dev/disk/by-id

vgchange -an || true

# Stop all mdadm arrays that the boot may have activated.
# FIXME: `--stop` does not seem to exist? has no effect
# mdadm --stop --scan
mdadm --manage /dev/md0 --stop

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
ARRAY <ignore> UUID=00000000:00000000:00000000:00000000' \
  >/etc/mdadm/mdadm.conf

##: --- PRIMARY LAYOUT --------------------------------------------------------

# FIXME: use sfdisk
function mkPrimaryLayout() {
  local nvme=$1

  sgdisk --zap-all "$nvme"
  parted_nice --script "$nvme" mklabel gpt
  # Wipe any previous RAID/ZFS signatures
  wipefs --all --force "$nvme"

  sgdisk -n1:0:+2M -t1:EF02 "$nvme" #  bios
  sgdisk -n2:0:+1G -t2:EF00 "$nvme" #  boot (efi)
  sgdisk -n3:0:0 -t3:8300 "$nvme"   #  root

  partprobe

  # Wait for all required devices to exist
  udevadm settle --timeout=5 --exit-if-exists="${nvme}p2"
  udevadm settle --timeout=5 --exit-if-exists="${nvme}p3"
}

for nvme in "${NVMEXX[@]}"; do
  mkPrimaryLayout "${nvme}"
done

mkfs.fat -F 32 -n boot "$BOOT_DEV"

# No real practical use except to match partition layout.
mkfs.fat -F 32 -n boot-2 "${NVME02}p${EFI_PART}"

# NOTE: The actual filesystem will be mounted just prior to NixOS installation.
mkdir -p /mnt/boot

##: --- 3: NixOS ---

mkfs.btrfs --force \
  --label nixos \
  --data single \
  --metadata raid1 \
  "${NVME01}p3" \
  "${NVME02}p3"

btrfs device scan

mkdir -p /mnt
mount -t btrfs LABEL=nixos /mnt

# Subvolume basename should begin with `@` to distinguish it from a normal path.
# <https://askubuntu.com/questions/987104/why-the-in-btrfs-subvolume-names>
btrfs subvolume create /mnt/@root
btrfs subvolume create /mnt/@store
btrfs subvolume create /mnt/@log
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@persist
btrfs subvolume create /mnt/@mysql
btrfs subvolume create /mnt/@postgres

# Create a read-only snapshot of the `@root` subvolume for impermanace.
btrfs subvolume snapshot -r /mnt/@root /mnt/@root-blank

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

btrfs subvolume list -a /mnt

##: --- SECONDARY LAYOUT -------------------------------------------------------

function mkSecondaryLayout() {
  local hdd=$1

  sgdisk --zap-all "$hdd"
  parted_nice --script "$hdd" mklabel gpt
  # Wipe any previous RAID/ZFS signatures
  wipefs --all --force "$hdd"

  sgdisk -n1:0:0 -t1:8300 "$hdd"

  partprobe

  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists="${hdd}1"
}

for hdd in "${HDDXX[@]}"; do
  mkSecondaryLayout "$hdd"
done

lsblk

mkfs.btrfs --force \
  --label local \
  --data raid0 \
  --metadata raid1 \
  "${HDD01}1" \
  "${HDD02}1"

udevadm trigger
btrfs device scan

mkdir -p "${LOCAL_PREFIX}"
mount -t btrfs LABEL=local "${LOCAL_PREFIX}"
btrfs subvolume create "${LOCAL_PREFIX}/@backups"
btrfs subvolume create "${LOCAL_PREFIX}/@bt-completed"
btrfs subvolume create "${LOCAL_PREFIX}/@bt-incoming"
btrfs subvolume create "${LOCAL_PREFIX}/@bt-metadata"
btrfs subvolume create "${LOCAL_PREFIX}/@bt-watch"
btrfs subvolume create "${LOCAL_PREFIX}/@media-incoming"
btrfs subvolume create "${LOCAL_PREFIX}/@media-outgoing"
btrfs subvolume list -a "${LOCAL_PREFIX}"
umount "${LOCAL_PREFIX}"

mount -t btrfs -o "subvol=@backups,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/backups"

mount -t btrfs -o "subvol=@bt-completed,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/data/torrents/completed"

mount -t btrfs -o "subvol=@bt-incoming,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/data/torrents/incoming"

mount -t btrfs -o "subvol=@bt-metadata,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/data/torrents/metadata"

mount -t btrfs -o "subvol=@bt-watch,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/data/torrents/watch"

mount -t btrfs -o "subvol=@media-incoming,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/media/incoming"

mount -t btrfs -o "subvol=@media-outgoing,${BTRFSOPTS}" LABEL="local" \
      "${LOCAL_PREFIX}/media/outgoing"

# Repeat mkdir since `/mnt` has since been umounted.
mkdir -p /mnt/boot
mount "$BOOT_DEV" /mnt/boot

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

echo "experimental-features = nix-command flakes" >>/etc/nix/nix.conf

nix-channel --add https://nixos.org/channels/nixos-23.05 nixpkgs
nix-channel --add https://nixos.org/channels/nixos-23.05 nixos
nix-channel --update

# Open a Nix shell with the NixOS installation dependencies.
nix-env -f '<nixpkgs>' -iA nixos-install-tools

###: PREPARE NIXOS CONFIGURATION ===============================================

git clone https://git.sr.ht/~montchr/dotfield /mnt/etc/nixos \
  -b add-moraine
ln -s /mnt/etc/nixos /mnt/etc/dotfield

nixos-generate-config --root /mnt

nix build "/mnt/etc/nixos#nixosConfigurations.${NEW_HOSTNAME}.config.system.build.toplevel"

export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels

# FIXME: it seems `--flake` fails when installed by this method
# > /nix/var/nix/profiles/system/sw/bin/bash: line 10: umount: command not found
# not only in the above example, but several other missing mount/umount errors
# PATH="$PATH:/usr/sbin:/sbin" NIX_PATH="$NIX_PATH" "$(which nixos-install)" \
#   --no-root-passwd \
#   --root /mnt \
#   --flake "/mnt/etc/nixos#${NEW_HOSTNAME}" \
#   --max-jobs "$(nproc)" \
#   --impure

PATH="$PATH:/usr/sbin:/sbin" NIX_PATH="$NIX_PATH" "$(which nixos-install)" \
  --no-root-passwd \
  --root /mnt \
  --max-jobs "$(nproc)"
