{
  config,
  lib,
  pkgs,
  ops,
  ...
}:
let
  key = config.dotfield.whoami.pgp;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  # Use our fork of these modules while still pending upstream changes.
  # FIXME: https://github.com/nix-community/home-manager/pull/2964
  #  imports = [(flake.inputs.home-manager-gpg-agent-darwin + "/modules/services/gpg-agent.nix")];
  # disabledModules = ["services/gpg-agent.nix"];

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
      pinentryPackage = if isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;
      sshKeys = [ key ];
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
        # TODO: there's a new and better one
        keyserver = "hkps://keys.openpgp.org";
        # keyserver hkps://keyserver.ubuntu.com:443
        # keyserver hkps://hkps.pool.sks-keyservers.net
        # keyserver hkps://pgp.ocf.berkeley.edu
      };
    };
  };
}
