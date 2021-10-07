{ config, lib, pkgs, ... }:
let
  mailDir = "${config.my.xdg.data}/mail";
  gmailAccount =
    { domain
    , accountName ? null
    , realName ? config.my.name
    , username ? "chris"
    , ...
    }:
    let address = "${username}@${domain}"; in
    {
      inherit address realName;

      flavor = "gmail.com";
      passwordCommand = "${pkgs.pass}/bin/pass Email/${domain}/${username}";

      mbsync = {
        enable = true;
        # create = "both";
        # expunge = "both";
        patterns = [ "*" "[Gmail]*" ];
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
  imports = [ ./options.nix ];

  my.env = {
    MAILDIR = mailDir;
  };

  my.user.packages = with pkgs; [ mu isync ];
  my.hm.accounts.email = {
    maildirBasePath = mailDir;
    accounts = {
      personal = gmailAccount { domain = "cdom.io"; };
      work = gmailAccount { domain = "alley.co"; };
    };
  };
}

## References:
# https://github.com/Emiller88/dotfiles/blob/5eaabedf1b141c80a8d32e1b496055231476f65e/modules/shell/mail.nix
