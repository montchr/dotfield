{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  key = (config.dotfield.whoami).pgp;
  inherit (config.lib.dag) entryAfter;

  passwordStorePath = config.xdg.dataHome + "/pass";
  passwordStoreRemoteUrl = "git@git.sr.ht:~montchr/password-store";
in
  lib.mkIf config.programs.gpg.enable {
    programs.password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts:
        with exts; [
          pass-import # https://github.com/roddhjav/pass-import
          pass-otp # https://github.com/tadfisher/pass-otp
          pass-update # https://github.com/roddhjav/pass-update
        ]);
      settings = {
        PASSWORD_STORE_DIR = passwordStorePath;
        PASSWORD_STORE_KEY = key;
      };
    };
    programs.browserpass.enable = true;
    programs.browserpass.browsers = ["firefox"];

    # Ensure the password store exists.
    #
    # Initially, I tried using `services.git-sync` to clone this automatically
    # but that didn't work -- perhaps I misunderstood its usage.
    home.activation.ensurePasswordStore = entryAfter ["writeBoundary"] ''
      if [[ ! -d "${passwordStorePath}" ]]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${passwordStoreRemoteUrl} ${passwordStorePath}
      fi
    '';

    # FIXME: option removed -- use git-sync instead
    # services.password-store-sync.enable = !isDarwin;

    # FIXME: needs further configuration... does not play well with 1password,
    # for example
    # services.pass-secret-service.enable = true;
  }
