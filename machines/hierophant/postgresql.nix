{ config, ... }:
let
  cfg = config.services.postgresqlBackup;
in
{
  services.postgresql.enable = true;
  services.postgresqlBackup = {
    enable = true;
    backupAll = true;
    compression = "zstd";
    compressionLevel = 11;
  };
  services.borgbackup.jobs."services-backup".paths = [ cfg.location ];
}
## References:
#
# - <https://github.com/foo-dogsquared/nixos-config/blob/master/hosts/plover/modules/services/postgresql.nix>
