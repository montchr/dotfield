# moraine

## Layout

Loosely based on TRaSH's advice [link](https://trash-guides.info/Hardlinks/How-to-setup-for/)

```shell-session
sudo chown -R $USER:$USER /data
sudo chmod -R a=,a+rX,u+w,g+w /data
```

```txt
.
├── mnt
│  └── silo             #  │  │  │  │  │ [5] │
└── srv                 #  │  │  │  │  │  │  │
   └── data             #  │  │  │  │  │  │  │
      ├── media         #  │  │  │  │  │  │  │
      │  ├── incoming   #  │  │  │ [3] │  │  │
      │  ├── library    #  │  │  │  │  │ [5] │
      │  │  ├── movies  #  │  │  │  │  │ [5] │
      │  │  ├── music   #  │  │  │  │  │ [5] │
      │  │  ├── other   #  │  │  │  │  │ [5] │
      │  │  └── tv      #  │  │  │  │  │ [5] │
      │  └── outgoing   #  │  │  │  │ [4] │  │
      └── torrents      #  │  │  │  │  │  │  │
         ├── complete   #  │  │ [2] │  │  │  │
         ├── incoming   #  │ [1] │  │  │  │  │
         ├── metadata   #  │ [1] │  │  │  │  │
         └── watch      # [0] │  │  │  │  │  │
            ├── load    # [0] │  │  │  │  │  │
            └── start   # [0] │  │  │  │  │  │
```

### `/mnt`

[FHS: 3.12. /mnt : Mount point for a temporarily mounted filesystem](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/ch03s12.html)

> This directory is provided so that the system administrator may
> temporarily mount a filesystem as needed. The content of this
> directory is a local issue and should not affect the manner in which
> any program is run.
>
> This directory must not be used by installation programs: a suitable
> temporary directory not in use by the system must be used instead.

### `/srv`

[FHS: 3.17. /srv : Data for services provided by this system](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/ch03s17.html)

> This main purpose of specifying this is so that users may find the
> location of the data files for a particular service, and so that
> services which require a single tree for readonly data, writable
> data and scripts (such as cgi scripts) can be reasonably placed.
> Data that is only of interest to a specific user should go in that
> users' home directory. If the directory and file structure of the
> data is not exposed to consumers, it should go in `/var/lib`.


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

```txt
$ sudo lsblk --fs

NAME        FSTYPE FSVER LABEL  UUID                                 FSAVAIL FSUSE% MOUNTPOINTS
sda
└─sda1      btrfs        local  3977e543-92a3-492d-92cc-20e79bf14654   29.1T     0% /mnt/local/downloads/torrents
                                                                                    /mnt/local/backups
                                                                                    /mnt/local/downloads/completed
                                                                                    /mnt/local/Media
sdb
└─sdb1      btrfs        local  3977e543-92a3-492d-92cc-20e79bf14654
nvme0n1
├─nvme0n1p1
├─nvme0n1p2 vfat   FAT32 boot   64F0-05C1
                972.7M     5% /boot
└─nvme0n1p3 btrfs        nixos  19574e5d-7a98-4182-8215-4632a7a82f77    1.9T     0% /home
                                                                                    /nix/store
                                                                                    /var/log
                                                                                    /nix
                                                                                    /persist
                                                                                    /
nvme1n1
├─nvme1n1p1
├─nvme1n1p2 vfat   FAT32 boot-2 66FE-10AC
└─nvme1n1p3 btrfs        nixos  19574e5d-7a98-4182-8215-4632a7a82f77
```

#### Subvolumes

```sh-session
$ sudo btrfs subvolume list /

ID 256 gen 14116 top level 5 path @root
ID 258 gen 12635 top level 5 path @store
ID 259 gen 14116 top level 5 path @log
ID 260 gen 14116 top level 5 path @home
ID 261 gen 10 top level 5 path @persist
ID 262 gen 11 top level 5 path @mysql
ID 263 gen 12 top level 5 path @postgres
ID 264 gen 13 top level 5 path @root-blank
ID 269 gen 29 top level 256 path srv
ID 270 gen 30 top level 256 path var/lib/portables
ID 271 gen 31 top level 256 path var/lib/machines
ID 272 gen 14094 top level 256 path tmp
```

## Errata

### References

- [TRaSH-Guides](https://trash-guides.info/)

### Potential issues with RealTek r8169 NIC

[Hang Up with Realtek r8169-r8168 NIC - Hetzner Docs](https://docs.hetzner.com/robot/dedicated-server/operating-systems/realtek-r8169-r8168-nic)

> The system loses the network connection because the network card hangs up. The
> TCP segmentation offload of the NICs is defective and must be deactivated.
