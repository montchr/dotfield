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


###: CONFIGURATION =======================================================

export NEW_HOSTNAME=moraine

##: --- Networking ---

export IPV4_ADDR=""
export IPV4_CIDR=""
export IPV4_GATEWAY=""

export IPV6_ADDR=""
export IPV6_SUBNET=""
export IPV6_CIDR=""
export IPV6_GATEWAY=""

# FIXME
printf "You need to set the networking vars!\n" && return 1

# Cloudflare nameservers.
export NSV4="1.1.1.1" # also 1.0.0.1
export NSV6="2606:4700:4700::1111"

##: --- Devices ---

export FSOPTS="defaults,x-mount.mkdir,noatime"
export BTRFSOPTS="${FSOPTS},compress=zstd"

export LOCAL_PREFIX="/mnt/local"

# boot/root
export NVME01=""
export NVME02=""

# local data
export HDD01=""
export HDD02=""

# FIXME
printf "You need to set the device names!\n" && return 1

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

# NOTE: untested!
function mountSubvol() {
  local name="$1"

  local target="${prefix}${path}"
  local opts="subvol=@${name},ssd,${FSOPTS}"

  if [[ "/" == "$path" ]]; then
    target="${prefix}"
  fi

  mount -t btrfs -o "${opts}" LABEL="nixos" "${target}"
}

# Inspect existing disks
lsblk
ls /dev/disk/by-id

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

for nvme in "${NVMEXX[@]}"; do
  # N.B. untested/aspirational!
  sgdisk --zap-all "$nvme"
  parted_nice --script "$nvme" mklabel gpt
  # Wipe any previous RAID/ZFS signatures
  wipefs --all --force "$nvme"

  sgdisk -n1:0:+4M -t1:EF02 "$nvme"  # bios
  sgdisk -n2:0:+2G -t2:EF00 "$nvme"  # efi/esp
  sgdisk -n3:0:0 -t3:8300 "$nvme"    # root

  partprobe

  mkfs.fat -F 32 -n EFI "${nvme}-part${EFI_PART}"
done

# NOTE: `--metadata` was originally `single` but the default `raid1` would be preferable,
#       as long as it doesn't interfere with boot.
mkfs.btrfs --force \
  --label nixos \
  --data single \
  "${NVME01}-part${ROOT_PART}" \
  "${NVME02}-part${ROOT_PART}"

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
mount "${NVME01}-part${BIOS_PART}" /mnt/boot


##: --- VOLUME: 'LOCAL' ----------------------------------------------------------

mkfs.btrfs --force \
  --label local \
  --data raid1 \
  --metadata raid1 \
  "${HDD01}-part1" \
  "${HDD02}-part1"

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
