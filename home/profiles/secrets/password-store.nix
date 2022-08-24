{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config.lib.dotfield.whoami) pgpPublicKey;
  passwordStorePath = config.xdg.dataHome + "/pass";
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
        PASSWORD_STORE_KEY = pgpPublicKey;
      };
    };
    programs.browserpass.enable = true;
    programs.browserpass.browsers = ["firefox"];

    services.password-store-sync.enable = true;
    services.password-store-sync.enable = !isDarwin;

    # FIXME: needs further configuration... does not play well with 1password,
    # for example
    # services.pass-secret-service.enable = true;
  }
