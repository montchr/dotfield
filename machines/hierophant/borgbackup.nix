{ flake, config, ... }:
let
  inherit (config.sops) secrets;
  inherit (flake.self.lib.backups) mkJob;
in
{
  services.borgbackup.jobs = {
    host-backup = mkJob {
      inherit secrets;
      paths = [ "/home" ];
      keyFile = "cat ${secrets."borg/ssh-key".path}";
      passCommand = "cat ${secrets."borg/repos/host/passphrase".path}";
      repo = "ssh://c7bv8x8g@c7bv8x8g.repo.borgbase.com/./repo";
    };
    services-backup = mkJob {
      inherit secrets;
      # TODO: probably others
      paths = [ "/var/lib/acme" ];
      keyFile = "cat ${secrets."borg/ssh-key".path}";
      passCommand = "cat ${secrets."borg/repos/services/passphrase".path}";
      repo = "ssh://t1dij3bh@t1dij3bh.repo.borgbase.com/./repo";
    };
  };

  sops.secrets."borg/repos/host/passphrase" = { };
  sops.secrets."borg/repos/services/passphrase" = { };
  sops.secrets."borg/ssh-key" = { };
}
