moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.whoami) pgpPublicKey;
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
        # FIXME: pinentry won't work in emacs for ssh, looks for terminal to
        # attach to with ncurses. however, pinentry will work for gpg signing
        # commits.
        enableSshSupport = true;
        sshKeys = [pgpPublicKey];
      };

      programs.gpg = {
        enable = true;

        mutableKeys = false;
        mutableTrust = false;

        # TODO: clean up the format/structure of these key files
        publicKeys = [
          {
            source = ../../secrets + "/gpg-${pgpPublicKey}.txt";
            trust = "ultimate";
          }
          {
            source = ../../secrets + "/gpg-0xF0B8FB42A7498482.txt";
            trust = "ultimate";
          }
        ];

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
