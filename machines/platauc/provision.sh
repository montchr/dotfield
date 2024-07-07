#!/usr/bin/env bash

export FSOPTS="defaults,x-mount.mkdir,noatime"
export BTRFSOPTS="${FSOPTS},compress=zstd"

sgdisk --delete 1 /dev/sda
sgdisk --largest-new 1 /dev/sda
partprobe /dev/sda

mkfs.btrfs --force --label nixos /dev/sda1
btrfs device scan

mkdir -p /mnt
mount -t btrfs LABEL=nixos /mnt

btrfs subvolume create /mnt/@root
btrfs subvolume create /mnt/@store
btrfs subvolume create /mnt/@log
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@persist

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

udevadm trigger

btrfs subvolume list -a /mnt
