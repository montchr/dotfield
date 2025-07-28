{ config, pkgs, ... }:
let
  inherit (config.dotfield) whoami;

  mkProtonMailAccount =
    {
      user,
      domain ? "proton.me",
      realName ? whoami.name,
      primary ? false,
    }:
    let

      address = "${user}@${domain}";
      passCmdFor = user: domain: "${pkgs.pass}/bin/pass ${address}/bridge/password";
    in
    {
      inherit address realName primary;

      userName = address;

      passwordCommand = passCmdFor user domain;

      imap = {
        host = "127.0.0.1";
        port = 1143;
        tls = {
          enable = true;
          useStartTls = true;
        };
      };

      smtp = {
        host = "127.0.0.1";
        port = 1025;
        # Insecure transmission is... okay...?
        tls.enable = false;
      };

      mbsync = {
        enable = true;
        create = "maildir";
        # Insecure auth on localhost is... okay...?
        extraConfig.account.AuthMechs = "LOGIN";
        # Prevent... mishaps...
        remove = "none";
        expunge = "none";
      };

      notmuch = {
        enable = true;
      };
    };
in

{
  imports = [ ../../proton-bridge.nix ];

  accounts.email.accounts."chmont-at-proton" = mkProtonMailAccount {
    user = "chmont";
    primary = true;
  };
}
