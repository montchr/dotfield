{ config, pkgs, ... }:
let
  inherit (config.dotfield.dotfield) whoami;

  passCmdFor = user: domain: "${pkgs.pass}/bin/pass Email/${domain}/${user}";

  mkProtonMailAccount =
    {
      user,
      domain ? "proton.me",
      realName ? whoami.fullName,
    }:
    let
      address = "${user}@${domain}";
    in
    {
      inherit address realName;

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
        # TODO: verify effect
        # remove = "none";
        # expunge = "both";
      };
    };
in

{
  accounts.email.accounts.primary = mkProtonMailAccount {
    userName = "chmont";
  };

}
