{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.dotfield.whoami) pgpPublicKey;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
  lib.mkIf ("" != pgpPublicKey) (lib.mkMerge [
    {
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
          # TODO: is this still necessary? only mention in DrDuh guide is for Windows
          reader-port = "Yubico Yubi";
        };

        # https://github.com/drduh/config/blob/master/gpg.conf
        # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Configuration-Options.html
        # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Esoteric-Options.html
        settings = {
          # Keyserver URL
          keyserver = "hkps://keys.openpgp.org";
          # keyserver hkps://keyserver.ubuntu.com:443
          # keyserver hkps://hkps.pool.sks-keyservers.net
          # keyserver hkps://pgp.ocf.berkeley.edu
        };
      };
    }
  ])