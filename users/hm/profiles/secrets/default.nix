{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.lib.our.whoami) keys;

  passVars = {
    PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
    PASSWORD_STORE_KEY = "${keys.pgp} 0xF0B8FB42A7498482";
  };
in {
  programs.password-store = {
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
