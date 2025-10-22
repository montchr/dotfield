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
      passwordStoreRemoteUrl = "git@codeberg.org:astratagem/password-store.git";
      passwordStorePath = config.programs.password-store.settings.PASSWORD_STORE_DIR;
    in
    {
      programs.password-store.settings = {
        PASSWORD_STORE_DIR = config.home.homeDirectory + "/.password-store";
      };

      services.pass-secret-service = {
        storePath = cfg.settings.PASSWORD_STORE_DIR;
      };

      # FIXME: fails with:
      # Oct 21 20:18:34 riebeck hm-activate-cdom[36069]: Cloning into '/home/cdom/.password-store'...
      # Oct 21 20:18:35 riebeck hm-activate-cdom[36069]: error: cannot run ssh: No such file or directory
      # Oct 21 20:18:35 riebeck hm-activate-cdom[36069]: fatal: unable to fork
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
