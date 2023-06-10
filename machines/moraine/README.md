# moraine

## Hardware Data

### Overview

```txt
   CPU1: AMD Ryzen 7 7700 8-Core Processor (Cores 16)
   Memory:  63458 MB
   Disk /dev/nvme0n1: 1024 GB (=> 953 GiB)
   Disk /dev/nvme1n1: 1024 GB (=> 953 GiB)
   Disk /dev/sda: 16 TB (=> 14 TiB)
   Disk /dev/sdb: 16 TB (=> 14 TiB)
   Total capacity 30 TiB with 4 Disks

Network data:
   eth0  LINK: yes
         MAC:  ***************
         IP:   ***************
         IPv6: 2a01:4f8:200:5047::2/64
         RealTek RTL-8169 Gigabit Ethernet driver
```

### Filesystems

```console
root@rescue ~ # mkfs.btrfs --force \
  --label nixos \
  --data single \
  "${NVME01}p${ROOT_PART}" \
  "${NVME02}p${ROOT_PART}"
btrfs-progs v5.10.1
See http://btrfs.wiki.kernel.org for more information.

Label:              nixos
UUID:               8063fc96-b895-4d2a-a70b-c9475f6613f2
Node size:          16384
Sector size:        4096
Filesystem size:    1.86TiB
Block group profiles:
  Data:             single            8.00MiB
  Metadata:         RAID1             1.00GiB
  System:           RAID1             8.00MiB
SSD detected:       yes
Incompat features:  extref, skinny-metadata
Runtime features:
Checksum:           crc32c
Number of devices:  2
Devices:
   ID        SIZE  PATH
    1   951.86GiB  /dev/nvme0n1p3
    2   951.86GiB  /dev/nvme1n1p3
```

```console
root@rescue ~ # mkfs.btrfs --force \
  --label local \
  --data raid0 \
  --metadata raid1 \
  "${HDD01}1" \
  "${HDD02}1"
btrfs-progs v5.10.1
See http://btrfs.wiki.kernel.org for more information.

Label:              local
UUID:               7548a4b1-600c-4840-b68f-8c314cbfa99b
Node size:          16384
Sector size:        4096
Filesystem size:    29.10TiB
Block group profiles:
  Data:             RAID0             2.00GiB
  Metadata:         RAID1             1.00GiB
  System:           RAID1             8.00MiB
SSD detected:       no
Incompat features:  extref, skinny-metadata
Runtime features:
Checksum:           crc32c
Number of devices:  2
Devices:
   ID        SIZE  PATH
    1    14.55TiB  /dev/sda1
    2    14.55TiB  /dev/sdb1
```

## Errata

### Potential issues with RealTek r8169 NIC

[Hang Up with Realtek r8169-r8168 NIC - Hetzner Docs](https://docs.hetzner.com/robot/dedicated-server/operating-systems/realtek-r8169-r8168-nic)

> The system loses the network connection because the network card hangs up. The
> TCP segmentation offload of the NICs is defective and must be deactivated.
