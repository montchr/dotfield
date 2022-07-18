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

export FSOPTS="defaults,x-mount.mkdir,noatime"
export BTRFSOPTS="${FSOPTS},compress=zstd"

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

# crypt
export ENCD01="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1W0A00HFVNG"
export ENCD02="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A02AFVNG"
export ENCD03="/dev/disk/by-id/ata-TOSHIBA_MG08ACA16TEY_X1J0A05YFVNG"

export nvmexx=(
  "$NVME1"
  "$NVME2"
)
export hddxx=(
  "$HDD01"
  "$HDD02"
  "$HDD03"
  "$HDD04"
  "$HDD05"
  "$HDD06"
  "$HDD07"
  "$ENCD01"
  "$ENCD02"
  "$ENCD03"
)
export siloxx=(
  "$HDD01"
  "$HDD02"
  "$HDD03"
  "$HDD04"
  "$HDD05"
  "$HDD06"
  "$HDD07"
)
export encdxx=(
  "$ENCD01"
  "$ENCD02"
  "$ENCD03"
)


##: --- Helper Functions ---

# Wrapper for parted >= 3.3 that does not exit 1 when it cannot inform
# the kernel of partitions changing (we use partprobe for that).
parted_nice() {
  parted "$@" 2> parted-stderr.txt || {
    grep "unable to inform the kernel of the change" parted-stderr.txt \
      || echo >&2 "Parted failed; stderr: $(< parted-stderr.txt)"
  }
}


###: FORMAT/PARTITION/MOUNT =======================================================

# Inspect existing disks
lsblk
ls /dev/disk/by-id

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

# format all hdds for silo + crypt
for hdd in "${hddxx[@]}"; do
  sgdisk --zap-all "$hdd"
  parted_nice --script "$hdd" mklabel gpt

  sgdisk -n1:0:0 -t1:8300 "$hdd"

  partprobe

  # Wipe any previous RAID/ZFS signatures
  mdadm --zero-superblock --force "${hdd}-part1" || true

  # Wait for all devices to exist
  udevadm settle --timeout=5 --exit-if-exists="${hdd}-part1"
done


# Creating file systems changes their UUIDs.
# Trigger udev so that the entries in /dev/disk/by-uuid get refreshed.
# `nixos-generate-config` depends on those being up-to-date.
# See https://github.com/NixOS/nixpkgs/issues/62444
udevadm trigger

mkdir -p /mnt/boot{-fallback,}
mount "${NVME1}-part2" /mnt/boot
# mount "${NVME2}-part2" /mnt/boot-fallback


##: --- VOLUME: 'NIXOS' ---

for nvme in "${nvmexx[@]}"; do
  # N.B. untested/aspirational!
  sgdisk --zap-all "$nvme"
  parted_nice --script "$nvme" mklabel gpt

  sgdisk -n1:0:+4M  -t1:EF02 "$nvme"  # bios
  sgdisk -n2:0:+2G  -t2:EF00 "$nvme"  # esp
  sgdisk -n3:0:+11G -t3:8200 "$nvme"  # swap
  sgdisk -n4:0:0    -t4:8300 "$nvme"  # root

  partprobe

  for i in {1..4}; do
    udevadm settle --timeout=5 --exit-if-exists="${nvme}-part${i}"
    # Wipe any previous RAID/ZFS signatures
    mdadm --zero-superblock --force "${nvme}-part${i}" || true
  done

  mkfs.fat -F 32 -n EFI "${nvme}-part2"
done


mkfs.btrfs --force \
  --data single \
  --metadata single \
  --label nixos \
    "${NVME1}-part4" \
    "${NVME2}-part4"

btrfs device scan

mkdir -p /mnt
mount -t btrfs LABEL=nixos /mnt

btrfs subvolume create /mnt/@root
btrfs subvolume create /mnt/@store
btrfs subvolume create /mnt/@log
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@persist
btrfs subvolume create /mnt/@postgres

btrfs subvolume list -a /mnt
umount /mnt

mount -t btrfs -o "subvol=@root,ssd,${FSOPTS}" \
  LABEL="nixos" \
  /mnt
mount -t btrfs -o "subvol=@store,ssd,${FSOPTS}" \
  LABEL="nixos" \
  /mnt/nix
mount -t btrfs -o "subvol=@log,ssd,${FSOPTS}" \
  LABEL="nixos" \
  /mnt/var/log
mount -t btrfs -o "subvol=@home,ssd,${FSOPTS}" \
  LABEL="nixos" \
  /mnt/home
mount -t btrfs -o "subvol=@persist,ssd,${FSOPTS}" \
  LABEL="nixos" \
  /mnt/persist
mount -t btrfs -o "subvol=@postgres,ssd,${FSOPTS}" \
  LABEL="nixos" \
  /mnt/var/lib/postgres


##: --- VOLUME: 'SILO' ---

mkfs.btrfs \
  --data raid10 \
  --metadata raid10 \
  --label silo \
    "${HDD01}-part1" \
    "${HDD02}-part1" \
    "${HDD03}-part1" \
    "${HDD04}-part1" \
    "${HDD05}-part1" \
    "${HDD06}-part1" \
    "${HDD07}-part1"

btrfs device scan

mkdir -p /mnt/silo

mount -t btrfs LABEL=silo /mnt/silo
btrfs subvolume create /mnt/silo/@backups
btrfs subvolume create /mnt/silo/@dl-completed
btrfs subvolume create /mnt/silo/@music
btrfs subvolume create /mnt/silo/@movies
btrfs subvolume create /mnt/silo/@tv-shows
btrfs subvolume list -a /mnt/silo
umount /mnt/silo

mount -t btrfs -o "subvol=@backups,${FSOPTS}" \
  LABEL="silo" \
  /mnt/silo/backups
mount -t btrfs -o "subvol=@dl-completed,${FSOPTS}" \
  LABEL="silo" \
  /mnt/silo/downloads/completed
mount -t btrfs -o "subvol=@music,${FSOPTS}" \
  LABEL="silo" \
  /mnt/silo/media/music
mount -t btrfs -o "subvol=@movies,${FSOPTS}" \
  LABEL="silo" \
  /mnt/silo/media/movies
mount -t btrfs -o "subvol=@tv-shows,${FSOPTS}" \
  LABEL="silo" \
  /mnt/silo/media/tv-shows


##: --- VOLUME: 'CRYPT' ---

# crypt
for i in {01..03}; do
  varName="ENCD${i}"
  encd="${!varName}"

  cryptsetup luksFormat --cipher aes-xts-plain64 "${encd}-part1"
  cryptsetup open --type luks "${encd}-part1" "crypt-${i}"
done

mkfs.btrfs \
  --data raid10 \
  --metadata raid10 \
  --label crypt \
    "${ENCD01}-part1" \
    "${ENCD02}-part1" \
    "${ENCD03}-part1"

btrfs device scan

mount -t btrfs LABEL=crypt /mnt/crypt
btrfs subvolume create /mnt/crypt/@crypted
btrfs subvolume list -a /mnt/crypt
umount /mnt/crypt

mount -t btrfs -o "subvol=@crypted,${FSOPTS}" \
  LABEL="crypt" \
  /mnt/crypt


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

# Installation fails without this.
mkdir -p /mnt/tmp

mkdir -p /mnt/etc/nixos
git clone https://git.sr.ht/~montchr/dotfield /mnt/etc/nixos

nixos-generate-config --root /mnt

nix build '/mnt/etc/nixos#nixosConfigurations.tapestone.config.system.build.toplevel'

# Pre-flight check to prevent issues with missing files during install.
# https://discourse.nixos.org/t/nixos-21-05-installation-failed-installing-from-an-existing-distro/13627/3
# https://github.com/NixOS/nixpkgs/issues/126141#issuecomment-861720372
# nix-build '<nixpkgs/nixos>' -A config.system.build.toplevel -I nixos-config=/mnt/etc/nixos/configuration.nix

# Install NixOS
PATH="$PATH" "$(command -v nixos-install)" \
  --no-root-passwd \
  --root /mnt \
  --max-jobs "$(nproc)"
