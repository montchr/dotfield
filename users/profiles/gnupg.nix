{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.dotfield.whoami) pgpPublicKey;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
  lib.mkIf ("" != pgpPublicKey) {
    home.sessionVariables.DOTFIELD_PGP_KEY = pgpPublicKey;

    home.packages = with pkgs; [
      gnupg
      gpgme

      (writeShellScriptBin "gpg-agent-restart" ''
        pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
      '')
    ];

    services.gpg-agent = {
      enable = true;
      enableScDaemon = true;
      enableSshSupport = true;
    };

    programs.gpg = {
      enable = true;

      scdaemonSettings = {
        disable-ccid = true;
        reader-port = "Yubico Yubi";
      };

      settings = {
        keyserver = "hkps://keys.openpgp.org";
      };
    };
  }
