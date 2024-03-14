{ config, ... }:
let
  inherit (config.sops) secrets;
  inherit (config.users) users groups;

  cfg = config.services.deluge;

  user = users.${cfg.user};
  baseDir = "/srv/data/torrents";
in
{
  users.users.${cfg.user}.extraGroups = [ groups."keys".name ];

  sops.secrets."services/deluge/auth-file" = {
    owner = user.name;
    inherit (user) group;
    # deluged will try to enforce this mode.
    # <https://git.deluge-torrent.org/deluge/tree/deluge/common.py#n1208>
    mode = "0400";
    path = "${cfg.dataDir}/auth-file";
    restartUnits = [
      "deluged.service"
      "delugeweb.service"
    ];
  };

  networking.firewall.allowedTCPPorts = [ cfg.config.daemon_port ];

  services.deluge = {
    enable = true;
    declarative = true;
    openFirewall = true;
    # NOTE: auth file requires `localclient:<password>:10`, at least on first run
    # TODO: this should be fixed or noted in the upstream NixOS module,
    #       though there is very little evidence explaining this requirement/oddity.
    authFile = secrets."services/deluge/auth-file".path;

    # TRaSH-Guides: <https://trash-guides.info/Downloaders/Deluge/Basic-Setup/>:
    # all options+defaults: <https://git.deluge-torrent.org/deluge/tree/deluge/core/preferencesmanager.py>
    config = {
      ##: downloads
      download_location = "${baseDir}/incoming";
      prioritize_first_last_pieces = true;
      pre_allocate_storage = true;
      torrentfiles_location = "${baseDir}/metadata";
      copy_torrent_file = true;
      move_completed_path = "${baseDir}/completed";
      move_completed = true;

      ##: plugins (by name)
      # <https://trash-guides.info/Downloaders/Deluge/Tips/#plugins>
      enabled_plugins = [
        # Enable watch directory support.
        "AutoAdd"
        "Execute"
        # High-volume libtorrent optimisations.
        # <https://trash-guides.info/Downloaders/Deluge/Tips/#ltconfig>
        # <https://forum.deluge-torrent.org/viewtopic.php?p=235653#p235653>
        # TODO: configure
        "ItConfig"
        "Label"
      ];

      ##: network (incoming aka "listening")
      listen_ports = [
        58112
        58112
      ];
      random_port = false;

      ##: network (outgoing)
      outgoing_ports = [
        0
        0
      ];
      random_outgoing_ports = true;

      ##: network (encryption)
      enc_in_policy = 1; # "Enabled"
      enc_out_policy = 1; # "Enabled"
      enc_level = 2; # "Full Stream"

      ##: network (extras)
      upnp = false;
      natpmp = false;
      dht = false;
      lsd = false;
      utpex = false;

      ##: bandwidth (global)
      max_connections_global = -1;
      max_upload_slots_global = -1;
      max_download_speed = -1;
      max_upload_speed = -1;
      max_half_open_connections = 128;
      max_connections_per_second = 128;
      ignore_limits_on_local_network = true;
      rate_limit_ip_overhead = true;

      ##: bandwidth (per-torrent)
      max_connections_per_torrent = -1;
      max_upload_slots_per_torrent = -1;
      max_download_speed_per_torrent = -1;
      max_upload_speed_per_torrent = -1;

      ##: queue
      dont_count_slow_torrents = true;
      queue_new_to_top = false;
      max_active_downloading = 8;
      # do not enforce tracker-hampering seed caps
      max_active_limit = -1;
      max_active_seeding = -1;
      remove_seed_at_ratio = false;
      seed_time_limit = -1;
      seed_time_ratio_limit = -1;

      ##: daemon
      allow_remote = true;
      daemon_port = 58846;

      ##: miscellaneous
      # disable sending analytics to deluge devs
      send_info = false;
      new_release_check = false;
    };
  };
}
