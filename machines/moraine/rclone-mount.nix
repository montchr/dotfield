# FIXME: set up rclone remote declaratively (somehow)
# TODO: sshfs or samba/cifs?
###  Sources:
# <https://github.com/nzbr/nixos/blob/0721f599a073e7bb5b3acf8a38bf415418188cbf/module/service/borgbackup.nix#L76C1-L105C11>
# <https://arne.me/writing/plex-on-nixos/>
# <https://rclone.org/commands/rclone_mount/#rclone-as-unix-mount-helper>
# <https://github.com/rclone/rclone/wiki/rclone-mount-helper-script>
# <https://github.com/rclone/rclone/wiki/Systemd-rclone-mount>
# <https://rclone.org/sftp/#hetzner-storage-box>
# <https://rclone.org/smb/>
# <https://docs.hetzner.com/robot/storage-box/access/access-overview>
# <https://docs.saltbox.dev/apps/hetzner_nfs/?h=hetzner>
# <https://docs.saltbox.dev/advanced/feeder/>
# <https://www.reddit.com/r/linuxquestions/comments/9gs0bm/samba_vs_nfs_vs_sshfs/>
# TODO: why not use `rclone union` remote type? saltbox does not seem to use this (why?) <https://rclone.org/union/>
# TODO: <https://github.com/saltyorg/Saltbox/blob/master/roles/remote/defaults/main.yml>
# TODO: <https://trash-guides.info/Hardlinks/Examples/>
# TODO: <https://github.com/saltyorg/Saltbox/blob/2a2d4ae8ea269526f5f5b5853876c106a9d22242/roles/unionfs/defaults/main.yml#L50-L55>
# TODO: <https://github.com/saltyorg/Saltbox/blob/2a2d4ae8ea269526f5f5b5853876c106a9d22242/roles/common/tasks/btrfs.yml#L30-L32>
#
# NOTE(rclone): Option --rc does not work together with --daemon or in mount.rclone mode. We are aware of this limitation and work on fixing it. It is tracked by tickets:
# https://github.com/rclone/rclone/issues/2618
# https://github.com/rclone/rclone/issues/5664
{pkgs, ...}: let
  rcloneRemote = "silo";
  runDir = "/srv/data/run";
  mediaDir = "srv/data/media/";
  # FIXME: should be unionfs
  mountPath = "${mediaDir}/library";

  # TODO: btrfs subvolume for cache?
  #       -- yea actually... what happens when it's NOT a subvol?
  #          i bet it falls back to the nvme `/` root vol, not the hdd vol...
  cachePath = "${mediaDir}/cache";
in {
  systemd.tmpfiles.rules = [
    # FIXME: adjust to needs
    "d ${cachePath} 0755 root root -"
  ];

  system.fsPackages = [
    # <https://rclone.org/commands/rclone_mount/#rclone-as-unix-mount-helper>
    (pkgs.runCommand "mount.rclone" {} ''
      mkdir -p $out/bin
      ln -s ${pkgs.rclone}/bin/rclone $out/bin/mount.rclone
    '')
  ];

  ################################
  # Feeder
  ################################

  # TODO:
  # feeder_mount_start_command: |-
  #   /usr/bin/rclone mount \
  #     --user-agent='{{ user_agent }}' \
  #     --config={{ rclone_config_path }} \
  #     --allow-other \
  #     --copy-links \
  #     --dir-cache-time=1m \
  #     --max-read-ahead=200M \
  #     --umask=002 \
  #     --syslog \
  #     -v \
  #     feeder:/mnt/local /mnt/feeder

  # NOTE: vfs mount
  #   rclone_vfs_mount_start_command: |-
  #   /usr/bin/rclone mount \
  #     --user-agent='{{ user_agent }}' \
  #     --config={{ rclone_config_path }} \
  #     --allow-other \
  #     --rc \
  #     --rc-no-auth \
  #     --rc-addr=localhost:5572 \
  #     --drive-skip-gdocs \
  #     --vfs-read-chunk-size=64M \
  #     --vfs-read-chunk-size-limit=2048M \
  #     --buffer-size=64M \
  #     --poll-interval=15s \
  #     --dir-cache-time=1000h \
  #     --timeout=10m \
  #     --drive-chunk-size=64M \
  #     --drive-pacer-min-sleep=10ms \
  #     --drive-pacer-burst=1000 \
  #     --umask=002 \
  #     --syslog \
  #     {{ '--bind=' + ansible_default_ipv4.address + ' ' if mounts.ipv4_only else '' }}-v \
  #     {{ rclone.remote }}: /mnt/remote

  # NOTE(rclone): > The umount operation can fail, for example when the
  #               > mountpoint is busy. When that happens, it is the user's
  #               > responsibility to stop the mount manually."
  # rclone_vfs_mount_stop_command: /bin/fusermount -uz /mnt/remote

  # rclone_vfs_mount_start_post_command: |-
  #   /usr/bin/rclone rc vfs/refresh recursive=true --url http://localhost:5572 _async=true

  # NOTE: vfs cache
  fileSystems."/mnt/silo" = {
    fsType = "rclone";
    # via <https://github.com/saltyorg/Saltbox/blob/master/roles/remote/defaults/main.yml>
    options = [
      "config=${rcloneConf}"
      "allow-other"
      "async-read=true"
      "dir-cache-time=1000h"
      "buffer-size=32M"
      "poll-interval=15s"
      "rc"
      "rc-no-auth"
      "rc-addr=localhost:5572"
      "use-mmap"
      "vfs-read-ahead=128M"
      "vfs-read-chunk-size=32M"
      "vfs-read-chunk-size-limit=2G"
      # FIXME:"vfs-cache-max-age=???????????????"
      "vfs-cache-mode=full"
      "vfs-cache-poll-interval=30s"
      # FIXME: "vfs-cache-max-size=???????????"
      "timeout=10m"
      # FIXME: verify umask
      "umask=002"
      "cache-dir=${vfsCacheDir}"
      # FIXME: maybe? "syslog"
      # FIXME: what? "bind=${ipv4} ..... if mounts.ipv4only else '' -v"
      # TODO: remove probably: "user-agent=????????";
      "${rcloneSiloRemote}: "
      # FIXME: prob unnecessary "mountpoint"
    ];
    # FIXME: translate above:
    # /usr/bin/rclone mount \
    # --user-agent='{{ user_agent }}' \
    # --config={{ rclone_config_path }} \
    # --allow-other \
    # --async-read=true \
    # --dir-cache-time=1000h \
    # --buffer-size=32M \
    # --poll-interval=15s \
    # --rc \
    # --rc-no-auth \
    # --rc-addr=localhost:5572 \
    # --use-mmap \
    # --vfs-read-ahead=128M \
    # --vfs-read-chunk-size=32M \
    # --vfs-read-chunk-size-limit=2G \
    # --vfs-cache-max-age={{ rclone_vfs_cache_max_age }} \
    # --vfs-cache-mode=full \
    # --vfs-cache-poll-interval=30s \
    # --vfs-cache-max-size={{ rclone_vfs_cache_max_size }} \
    # --timeout=10m \
    # --drive-skip-gdocs \
    # --drive-pacer-min-sleep=10ms \
    # --drive-pacer-burst=1000 \
    # --umask=002 \
    # {{ '--cache-dir=' + rclone_vfs_cache_dir + ' ' if (rclone_vfs_cache_dir | length > 0) else "" }}--syslog \
    # {{ '--bind=' + ansible_default_ipv4.address + ' ' if mounts.ipv4_only else "" }}-v \
    # {{ rclone.remote }}: /mnt/remote
  };

  # TODO:
  #   rclone_vfs_cache_mount_stop_command: /bin/fusermount -uz /mnt/remote

  # TODO:
  # rclone_vfs_cache_mount_start_post_command: |-
  #   /usr/bin/rclone rc vfs/refresh recursive=true --url http://localhost:5572 _async=true

  # TODO:
  #   rclone_vfs_refresh_interval: 86400
  # rclone_vfs_refresh_command: |-
  #   /usr/bin/rclone rc vfs/refresh recursive=true --url http://localhost:5572 _async=true

  #############################################################################
  # TODO: <https://github.com/saltyorg/Saltbox/blob/2a2d4ae8ea269526f5f5b5853876c106a9d22242/roles/unionfs/defaults/main.yml#L50-L55>

  #   local_mount_branch: "{{ '/mnt/feeder' if (feeder_remote_is_active is defined and feeder_remote_is_active) else '/mnt/local' }}"

  # mount_type: "mergerfs"

  #   mergerfs_mount_branches: "{{ local_mount_branch }}=RW:/mnt/remote=NC"

  # mergerfs_mount_service_after: "network-online.target"

  # mergerfs_mount_start_command: |-
  #   /usr/bin/mergerfs \
  #     -o category.create=ff,async_read=true,cache.files=partial \
  #     -o dropcacheonclose=true,minfreespace=0,fsname=mergerfs \
  #     -o xattr=nosys,statfs=base,statfs_ignore=nc,umask=002,noatime \
  #     {{ mergerfs_mount_branches }} /mnt/unionfs

  # mergerfs_mount_stop_command: /bin/fusermount -u /mnt/unionfs

  ###################################################################

  # FIXME: set mode/group
  # <https://trash-guides.info/Hardlinks/How-to-setup-for/Native/#permissions>
  fileSystems.${mountPath} = {
    # NOTE: Ends with `:`.
    # <https://rclone.org/commands/rclone_mount/#rclone-as-unix-mount-helper>
    device = "${rcloneRemote}:";
    fsType = "rclone";
    options = [
      "user"
      "noauto"
      "_netdev"
      "rw"
      "allow_other"
      "args2env"
      "vfs-cache-mode=writes"
      "vfs-cache-max-size=1G"
      "config=/root/.config/rclone/rclone.conf"
      "cache-dir=${cachePath}"
      "log-file=${runDir}/rclone.log"
      "x-systemd.automount"
      "x-systemd.mount-timeout=5"
      "x-systemd.idle-timeout=30"
    ];
  };

  environment.systemPackages = [pkgs.rclone];
}
