{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.lib.our) dotfieldPath whoami;

  passVars = {
    PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
    PASSWORD_STORE_KEY = "${whoami.keys.pgp} 0xF0B8FB42A7498482";
  };
in {
  home.packages = with pkgs; [
    yubikey-manager
    yubikey-personalization
  ];

  home.sessionVariables.AGENIX_ROOT = dotfieldPath;

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
