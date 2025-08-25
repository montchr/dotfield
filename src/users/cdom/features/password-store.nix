flake@{ lib, ... }:
{
  dotfield.users.cdom.aspects.workstation.home =
    { config, pkgs, ... }:
    let
      inherit (flake.config.dotfield.meta.users.${config.username}) whoami;
      inherit (config.lib.dag) entryAfter;

      cfg = config.programs.password-store;
      passwordStorePath = config.home.homeDirectory + "/.password-store";
      passwordStoreRemoteUrl = "git@codeberg.org:astratagem/password-store";
    in
    lib.mkIf config.programs.password-store.enable {
      programs.password-store.settings = {
        "PASSWORD_STORE_DIR" = passwordStorePath;
        "PASSWORD_STORE_KEY" = whoami.pgp.id;
      };

      services.pass-secret-service.storePath = cfg.settings."PASSWORD_STORE_DIR";

      # Ensure the password store exists.
      home.activation.ensurePasswordStore = entryAfter [ "writeBoundary" ] ''
        if [[ ! -d "${passwordStorePath}" ]]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${passwordStoreRemoteUrl} ${passwordStorePath}
        fi
      '';

      # Sync changes to remote.
      services.git-sync.enable = true;
      services.git-sync.repositories."password-store" = {
        uri = passwordStoreRemoteUrl;
        path = passwordStorePath;
        interval = 300;
      };
    };
}
