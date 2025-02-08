{ config, pkgs, ... }:
{
  accounts.email.accounts."TU" = {
    address = "chrismont@temple.edu";
    realName = "chris montgomery";
    userName = "tuc26080@temple.edu";
    passwordCommand = "pizauth show TU";
    imap = {
      port = 993;
      tls.enable = true;
    };
    mbsync = {
      enable = true;
      create = "maildir";
      extraConfig.account.AuthMechs = "XOAUTH2";
      remove = "none";
      expunge = "none";
    };
    notmuch.enable = true;

  };

}
