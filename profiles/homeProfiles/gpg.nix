{
  config,
  lib,
  pkgs,
  flake,
  ops,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  key = (config.dotfield.whoami).pgp;
in {
  config = lib.mkIf ("" != key) {
    home.sessionVariables.DOTFIELD_PGP_KEY = key;

    home.packages = with pkgs; [
      gnupg
      gpgme

      (writeShellScriptBin "gpg-agent-restart" ''
        pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
      '')
    ];

    services.gpg-agent = {
      enable = true;
      enableSshSupport = false;
      # TODO: still necessary?
      pinentryFlavor = lib.mkIf isDarwin "mac";
      sshKeys = [key];
    };

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      publicKeys = [
        {
          text = ops.keys.pgp.asc.${key};
          trust = "ultimate";
        }
        {
          text = ops.keys.pgp.asc."0xF0B8FB42A7498482";
          trust = "ultimate";
        }
      ];

      # https://github.com/drduh/config/blob/master/gpg.conf
      # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Configuration-Options.html
      # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Esoteric-Options.html
      settings = {
        # Keyserver URL
        # TODO: some of these might be dead
        keyserver = "hkps://keys.openpgp.org";
        # keyserver hkps://keyserver.ubuntu.com:443
        # keyserver hkps://hkps.pool.sks-keyservers.net
        # keyserver hkps://pgp.ocf.berkeley.edu
      };
    };
  };
}
