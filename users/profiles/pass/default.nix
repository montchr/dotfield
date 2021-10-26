{ config, lib, pkgs, ... }:
let
  inherit (config.my) xdg;

  passVars = { PASSWORD_STORE_DIR = "${xdg.data}/pass"; };
in
{
  environment.variables = passVars;
  my.hm.programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: with exts; [
      pass-import # https://github.com/roddhjav/pass-import
      pass-otp # https://github.com/tadfisher/pass-otp
      pass-update # https://github.com/roddhjav/pass-update
    ]);
    settings = passVars;
  };
}
