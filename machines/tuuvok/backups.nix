{ flake, config, ... }:
let
  inherit (config.networking) hostName;
  inherit (config.sops) secrets;
  inherit (flake.self.lib.backups) mkJob;
in
{
  services.borgbackup.jobs = {
    home-backup = mkJob {
      startAt = "hourly";
      paths = [ "/home" ];
      keyFile = secrets."borg/repos/home/ssh-key".path;
      passCommand = "cat ${secrets."borg/repos/home/passphrase".path}";
      repo = "ssh://de1954@de1954.rsync.net/./backups/${hostName}-home";
    };

    # services-backup = mkJob {
    #   inherit secrets;
    #   # TODO: probably others
    #   paths = [ "/var/lib/acme" ];
    #   passCommand = "cat ${secrets."borg/repos/services/passphrase".path}";
    #   # repo = "ssh://t1dij3bh@t1dij3bh.repo.borgbase.com/./repo";
    # };
  };

  sops.secrets = {
    "borg/repos/home/passphrase" = { };
    "borg/repos/home/ssh-key" = { };
    # "borg/repos/services/passphrase" = { };
    # "borg/repos/services/ssh-key" = { };
  };
}
