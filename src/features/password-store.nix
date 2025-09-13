flake@{ ... }:
{
  aspects.workstation.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;
      key = whoami.pgp.id;
      cfg = config.programs.password-store;
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
          PASSWORD_STORE_KEY = key;
        };
      };

      programs.browserpass.enable = true;
      programs.browserpass.browsers = [ "firefox" ];
    };
}
