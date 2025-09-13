{
  users.cdom.aspects.workstation.home =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      inherit (lib.hm.dag) entryAfter;
      cfg = config.programs.password-store;
      passwordStoreRemoteUrl = "git@codeberg.org:montchr/password-store";
      passwordStorePath = config.programs.password-store.settings.PASSWORD_STORE_DIR;
    in
    {
      programs.password-store.settings = {
        PASSWORD_STORE_DIR = config.home.homeDirectory + "/.password-store";
      };

      services.pass-secret-service = {
        storePath = cfg.settings.PASSWORD_STORE_DIR;
      };

      home.activation.ensurePasswordStore = entryAfter [ "writeBoundary" ] ''
          if [[ ! -d "${passwordStorePath}" ]]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${passwordStoreRemoteUrl} ${passwordStorePath}
        fi
      '';

      services.git-sync.repositories."password-store" = {
        uri = passwordStoreRemoteUrl;
        path = passwordStorePath;
        interval = 300;
      };
    };
}
