{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.dotfield.whoami) pgpPublicKey;

  dotfieldPath = "${config.xdg.configHome}/dotfield";
in
  lib.mkMerge [
    {
      home.packages = with pkgs; [
        yubikey-manager
        yubikey-personalization
      ];

      home.sessionVariables.AGENIX_ROOT = dotfieldPath;

      programs.password-store = lib.mkIf config.programs.gpg.enable {
        enable = true;
        package = pkgs.pass.withExtensions (exts:
          with exts; [
            pass-import # https://github.com/roddhjav/pass-import
            pass-otp # https://github.com/tadfisher/pass-otp
            pass-update # https://github.com/roddhjav/pass-update
          ]);
        settings = {
          PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
          PASSWORD_STORE_KEY = "${pgpPublicKey} 0xF0B8FB42A7498482";
        };
      };
    }
    # FIXME: no effect?
    (lib.mkIf (config.programs.password-store.enable && config.programs.firefox.enable) {
      programs.browserpass.enable = true;
      programs.browserpass.browsers = ["firefox"];
    })
  ]
