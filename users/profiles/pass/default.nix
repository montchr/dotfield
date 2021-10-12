{ config, lib, pkgs, ... }:

{
  my.hm.programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: with exts; [
      pass-import # https://github.com/roddhjav/pass-import
      pass-otp # https://github.com/tadfisher/pass-otp
      pass-update # https://github.com/roddhjav/pass-update
    ]);
    settings = {
      PASSWORD_STORE_DIR = "${config.my.xdg.data}/pass";
    };
  };
}
