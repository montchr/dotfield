{
  flake,
  config,
  pkgs,
  ...
}:
let
  inherit (flake.config.meta.users.${config.home.username}) whoami;

  mkProtonMailAccount =
    {
      user,
      domain ? "proton.me",
      realName ? whoami.name,
      primary ? false,
    }:
    let
      address = "${user}@${domain}";
      tls = {
        enable = true;
        useStartTls = true;
        # The certificate will need to be exported by the Bridge
        # application in its Advanced Settings section.
        certificatesFile = "${config.xdg.configHome}/protonmail/cert.pem";
      };
    in
    {
      inherit address realName primary;
      userName = address;
      passwordCommand = "pass show ${address}/bridge/password";

      imap = {
        inherit tls;
        host = "127.0.0.1";
        port = 1143;
      };

      smtp = {
        inherit tls;
        host = "127.0.0.1";
        port = 1025;
      };

      mbsync = {
        enable = true;
        create = "maildir";
      };

      notmuch = {
        enable = true;
      };
    };
in

{
  imports = [ ../../proton-bridge.nix ];

  accounts.email.accounts."chmont-at-protonmail" = mkProtonMailAccount {
    user = "chmont";
    #    domain = "protonmail.com";
    primary = true;
  };
}
