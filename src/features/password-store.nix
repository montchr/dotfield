{ self, lib, ... }:
{
  dotfield.aspects.workstation.home =
    { config, pkgs, ... }:
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
      };

      programs.browserpass.enable = true;
      programs.browserpass.browsers = [ "firefox" ];
    };
}
