{ config, lib, pkgs, ... }:
let
  mailDir = "${config.my.xdg.data}/mail";
  gmailAccount =
    { name
    , domain
    , realName ? config.my.name
    , username ? "chris"
    , ...
    }:
    let address = "${username}@${domain}"; in
    {
      inherit address realName;

      flavor = "gmail.com";
      passwordCommand = "${pkgs.pass}/bin/pass Email/${domain}/${username}";

      mu.enable = true;

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        patterns = [ "*" "[Gmail]*" ];
        groups.${name} = {
          channels = {
            inbox.patterns = [ "INBOX" ];
            trash = {
              farPattern = "[Gmail]/Bin";
              nearPattern = "[Gmail].Bin";
            };
            sent = {
              farPattern = "[Gmail]/Sent Mail";
              nearPattern = "[Gmail].Sent Mail";
            };
            all = {
              farPattern = "[Gmail]/All Mail";
              nearPattern = "[Gmail].All Mail";
            };
            starred = {
              farPattern = "[Gmail]/Starred";
              nearPattern = "[Gmail].Starred";
            };
          };
        };
      };

      # https://tecosaur.github.io/emacs-config/config.html#fetching
      # TODO: for faster sync. needs configuration.
      # imapnotify = {
      #   enable = false;
      #   boxes = [ "Inbox" ];
      # };
    };
in
{
  my.env = {
    MAILDIR = mailDir;
  };

  my.user.packages = with pkgs; [ mu isync ];
  my.hm.accounts.email = {
    maildirBasePath = mailDir;
    accounts = {
      personal = gmailAccount {
        name = "personal";
        domain = "cdom.io";
      };
      work = gmailAccount {
        name = "work";
        domain = "alley.co";
      };
    };
  };
}

## References:
# https://github.com/Emiller88/dotfiles/blob/5eaabedf1b141c80a8d32e1b496055231476f65e/modules/shell/mail.nix
