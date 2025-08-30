{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake.config.meta.users.${config.home.username}) whoami;
  key = whoami.pgp.id;
  inherit (config.lib.dag) entryAfter;

  passwordStorePath = config.home.homeDirectory + "/.password-store";
  passwordStoreRemoteUrl = "git@codeberg.org:montchr/password-store";
in
lib.mkIf config.programs.gpg.enable {
  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (
      exts: with exts; [
        pass-import # https://github.com/roddhjav/pass-import
        pass-otp # https://github.com/tadfisher/pass-otp
        pass-update # https://github.com/roddhjav/pass-update
      ]
    );
    settings = {
      PASSWORD_STORE_DIR = passwordStorePath;
      PASSWORD_STORE_KEY = key;
    };
  };

  services.pass-secret-service = {
    enable = true;
    storePath = passwordStorePath;
  };

  programs.browserpass.enable = true;
  programs.browserpass.browsers = [ "firefox" ];

  # Ensure the password store exists.
  home.activation.ensurePasswordStore = entryAfter [ "writeBoundary" ] ''
    if [[ ! -d "${passwordStorePath}" ]]; then
      $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${passwordStoreRemoteUrl} ${passwordStorePath}
    fi
  '';

  # Sync changes to remote.
  services.git-sync.repositories."password-store" = {
    uri = passwordStoreRemoteUrl;
    path = passwordStorePath;
    interval = 300;
  };
}
