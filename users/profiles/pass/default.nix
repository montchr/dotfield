{
  config,
  lib,
  pkgs,
  ...
}: let
  hmConfig = config.home-manager.users.${config.my.username};
  passVars = {
    PASSWORD_STORE_DIR = "${hmConfig.xdg.dataHome}/pass";
    PASSWORD_STORE_KEY = "${config.my.keys.pgp} 0xF0B8FB42A7498482";
  };
in {
  environment.variables = passVars;
  my.hm.programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts:
      with exts; [
        pass-import # https://github.com/roddhjav/pass-import
        pass-otp # https://github.com/tadfisher/pass-otp
        pass-update # https://github.com/roddhjav/pass-update
      ]);
    settings = passVars;
  };
}
